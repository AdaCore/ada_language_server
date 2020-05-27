--  Automatically generated, do not edit.
with GNATCOLL.JSON;

with LSP.JSON_Streams;
with LSP.Messages;                 use LSP.Messages;
with LSP.Types;                    use LSP.Types;

package body LSP.Message_IO is
   pragma Style_Checks ("M175");

   procedure Read_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RequestMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("jsonrpc");
      LSP.Types.Read (S, V.jsonrpc);
      JS.Key ("id");
      LSP_Number_Or_String'Read (S, V.id);
      JS.Key ("method");
      LSP.Types.Read (S, V.method);
      JS.End_Object;
   end Read_RequestMessage;

   procedure Write_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RequestMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("jsonrpc");
      LSP.Types.Write (S, V.jsonrpc);
      JS.Key ("id");
      LSP_Number_Or_String'Write (S, V.id);
      JS.Key ("method");
      LSP.Types.Write (S, V.method);
      JS.End_Object;
   end Write_RequestMessage;

   procedure Read_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out NotificationMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("jsonrpc");
      LSP.Types.Read (S, V.jsonrpc);
      JS.Key ("method");
      LSP.Types.Read (S, V.method);
      JS.End_Object;
   end Read_NotificationMessage;

   procedure Write_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : NotificationMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("jsonrpc");
      LSP.Types.Write (S, V.jsonrpc);
      JS.Key ("method");
      LSP.Types.Write (S, V.method);
      JS.End_Object;
   end Write_NotificationMessage;

   procedure Read_CancelParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CancelParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("id");
      LSP_Number_Or_String'Read (S, V.id);
      JS.End_Object;
   end Read_CancelParams;

   procedure Write_CancelParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CancelParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("id");
      LSP_Number_Or_String'Write (S, V.id);
      JS.End_Object;
   end Write_CancelParams;

   procedure Read_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Position)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("line");
      Line_Number'Read (S, V.line);
      JS.Key ("character");
      UTF_16_Index'Read (S, V.character);
      JS.End_Object;
   end Read_Position;

   procedure Write_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Position)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("line");
      Line_Number'Write (S, V.line);
      JS.Key ("character");
      UTF_16_Index'Write (S, V.character);
      JS.End_Object;
   end Write_Position;

   procedure Read_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Span)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("start");
      Position'Read (S, V.first);
      JS.Key ("end");
      Position'Read (S, V.last);
      JS.End_Object;
   end Read_Span;

   procedure Write_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Span)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("start");
      Position'Write (S, V.first);
      JS.Key ("end");
      Position'Write (S, V.last);
      JS.End_Object;
   end Write_Span;

   procedure Read_CodeActionKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String := JS.Read.Get;
   begin
      if Text = "" then
         V := Empty;
      elsif Text = "quickfix" then
         V := QuickFix;
      elsif Text = "refactor" then
         V := Refactor;
      elsif Text = "refactor.extract" then
         V := RefactorExtract;
      elsif Text = "refactor.inline" then
         V := RefactorInline;
      elsif Text = "refactor.rewrite" then
         V := RefactorRewrite;
      elsif Text = "source" then
         V := Source;
      elsif Text = "source.organizeImports" then
         V := SourceOrganizeImports;
      else
         V := CodeActionKind'First;
      end if;
   end Read_CodeActionKind;

   procedure Write_CodeActionKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_String
        (Value : CodeActionKind)
         return GNATCOLL.JSON.UTF8_String;

      function To_String
        (Value : CodeActionKind)
         return GNATCOLL.JSON.UTF8_String is
      begin
         case Value is
            when Empty =>
               return "";
            when QuickFix =>
               return "quickfix";
            when Refactor =>
               return "refactor";
            when RefactorExtract =>
               return "refactor.extract";
            when RefactorInline =>
               return "refactor.inline";
            when RefactorRewrite =>
               return "refactor.rewrite";
            when Source =>
               return "source";
            when SourceOrganizeImports =>
               return "source.organizeImports";
         end case;
      end To_String;

   begin
      JS.Write (GNATCOLL.JSON.Create (To_String (V)));
   end Write_CodeActionKind;

   procedure Read_AlsReferenceKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AlsReferenceKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String := JS.Read.Get;
   begin
      if Text = "reference" then
         V := Simple;
      elsif Text = "access" then
         V := Access_Ref;
      elsif Text = "write" then
         V := Write;
      elsif Text = "call" then
         V := Static_Call;
      elsif Text = "dispatching call" then
         V := Dispatching_Call;
      elsif Text = "parent" then
         V := Parent;
      elsif Text = "child" then
         V := Child;
      else
         V := AlsReferenceKind'First;
      end if;
   end Read_AlsReferenceKind;

   procedure Write_AlsReferenceKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AlsReferenceKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_String
        (Value : AlsReferenceKind)
         return GNATCOLL.JSON.UTF8_String;

      function To_String
        (Value : AlsReferenceKind)
         return GNATCOLL.JSON.UTF8_String is
      begin
         case Value is
            when Simple =>
               return "reference";
            when Access_Ref =>
               return "access";
            when Write =>
               return "write";
            when Static_Call =>
               return "call";
            when Dispatching_Call =>
               return "dispatching call";
            when Parent =>
               return "parent";
            when Child =>
               return "child";
         end case;
      end To_String;

   begin
      JS.Write (GNATCOLL.JSON.Create (To_String (V)));
   end Write_AlsReferenceKind;

   procedure Read_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Location)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Read (S, V.uri);
      JS.Key ("range");
      LSP.Messages.Span'Read (S, V.span);
      JS.Key ("alsKind");
      AlsReferenceKind_Set'Read (S, V.alsKind);
      JS.End_Object;
   end Read_Location;

   procedure Write_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write (S, V.uri);
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("alsKind");
      AlsReferenceKind_Set'Write (S, V.alsKind);
      JS.End_Object;
   end Write_Location;

   procedure Read_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LocationLink)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("originSelectionRange");
      Optional_Span'Read (S, V.originSelectionRange);
      JS.Key ("targetUri");
      LSP.Types.Read (S, V.targetUri);
      JS.Key ("targetRange");
      Span'Read (S, V.targetRange);
      JS.Key ("targetSelectionRange");
      Span'Read (S, V.targetSelectionRange);
      JS.Key ("alsKind");
      AlsReferenceKind_Set'Read (S, V.alsKind);
      JS.End_Object;
   end Read_LocationLink;

   procedure Write_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LocationLink)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("originSelectionRange");
      Optional_Span'Write (S, V.originSelectionRange);
      JS.Key ("targetUri");
      LSP.Types.Write (S, V.targetUri);
      JS.Key ("targetRange");
      Span'Write (S, V.targetRange);
      JS.Key ("targetSelectionRange");
      Span'Write (S, V.targetSelectionRange);
      JS.Key ("alsKind");
      AlsReferenceKind_Set'Write (S, V.alsKind);
      JS.End_Object;
   end Write_LocationLink;

   procedure Read_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticSeverity)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := DiagnosticSeverity'Val (JS.Read.Get - 1);
   end Read_DiagnosticSeverity;

   procedure Write_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticSeverity)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(DiagnosticSeverity'Pos (V)) + 1));
   end Write_DiagnosticSeverity;

   procedure Read_DiagnosticTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticTag)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := DiagnosticTag'Val (JS.Read.Get - 1);
   end Read_DiagnosticTag;

   procedure Write_DiagnosticTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticTag)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(DiagnosticTag'Pos (V)) + 1));
   end Write_DiagnosticTag;

   procedure Read_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticRelatedInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("location");
      LSP.Messages.Location'Read (S, V.location);
      JS.Key ("message");
      LSP.Types.Read (S, V.message);
      JS.End_Object;
   end Read_DiagnosticRelatedInformation;

   procedure Write_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticRelatedInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("location");
      LSP.Messages.Location'Write (S, V.location);
      JS.Key ("message");
      LSP.Types.Write (S, V.message);
      JS.End_Object;
   end Write_DiagnosticRelatedInformation;

   procedure Read_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Diagnostic)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Read (S, V.span);
      JS.Key ("severity");
      Optional_DiagnosticSeverity'Read (S, V.severity);
      JS.Key ("code");
      LSP_Number_Or_String'Read (S, V.code);
      JS.Key ("source");
      Optional_String'Read (S, V.source);
      JS.Key ("message");
      LSP.Types.Read (S, V.message);
      JS.Key ("tags");
      Optional_DiagnosticTagSet'Read (S, V.tags);
      JS.Key ("relatedInformation");
      DiagnosticRelatedInformation_Vector'Read (S, V.relatedInformation);
      JS.End_Object;
   end Read_Diagnostic;

   procedure Write_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Diagnostic)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("severity");
      Optional_DiagnosticSeverity'Write (S, V.severity);
      JS.Key ("code");
      LSP_Number_Or_String'Write (S, V.code);
      JS.Key ("source");
      Optional_String'Write (S, V.source);
      JS.Key ("message");
      LSP.Types.Write (S, V.message);
      JS.Key ("tags");
      Optional_DiagnosticTagSet'Write (S, V.tags);
      JS.Key ("relatedInformation");
      DiagnosticRelatedInformation_Vector'Write (S, V.relatedInformation);
      JS.End_Object;
   end Write_Diagnostic;

   procedure Read_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Read (S, V.span);
      JS.Key ("newText");
      LSP.Types.Read (S, V.newText);
      JS.End_Object;
   end Read_TextEdit;

   procedure Write_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("newText");
      LSP.Types.Write (S, V.newText);
      JS.End_Object;
   end Write_TextEdit;

   procedure Read_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Read (S, V.uri);
      JS.End_Object;
   end Read_TextDocumentIdentifier;

   procedure Write_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write (S, V.uri);
      JS.End_Object;
   end Write_TextDocumentIdentifier;

   procedure Read_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("edits");
      TextEdit_Vector'Read (S, V.edits);
      JS.End_Object;
   end Read_TextDocumentEdit;

   procedure Write_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("edits");
      TextEdit_Vector'Write (S, V.edits);
      JS.End_Object;
   end Write_TextDocumentEdit;

   procedure Read_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Read (S, V.uri);
      JS.Key ("languageId");
      LSP.Types.Read (S, V.languageId);
      JS.Key ("version");
      Version_Id'Read (S, V.version);
      JS.Key ("text");
      LSP.Types.Read (S, V.text);
      JS.End_Object;
   end Read_TextDocumentItem;

   procedure Write_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write (S, V.uri);
      JS.Key ("languageId");
      LSP.Types.Write (S, V.languageId);
      JS.Key ("version");
      Version_Id'Write (S, V.version);
      JS.Key ("text");
      LSP.Types.Write (S, V.text);
      JS.End_Object;
   end Write_TextDocumentItem;

   procedure Read_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentPositionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Read (S, V.position);
      JS.End_Object;
   end Read_TextDocumentPositionParams;

   procedure Write_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentPositionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.End_Object;
   end Write_TextDocumentPositionParams;

   procedure Read_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out dynamicRegistration)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Read (S, V.dynamicRegistration);
      JS.End_Object;
   end Read_dynamicRegistration;

   procedure Write_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : dynamicRegistration)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.End_Object;
   end Write_dynamicRegistration;

   procedure Read_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ResourceOperationKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String := JS.Read.Get;
   begin
      if Text = "create" then
         V := create;
      elsif Text = "rename" then
         V := rename;
      elsif Text = "delete" then
         V := delete;
      else
         V := ResourceOperationKind'First;
      end if;
   end Read_ResourceOperationKind;

   procedure Write_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResourceOperationKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_String
        (Value : ResourceOperationKind)
         return GNATCOLL.JSON.UTF8_String;

      function To_String
        (Value : ResourceOperationKind)
         return GNATCOLL.JSON.UTF8_String is
      begin
         case Value is
            when create =>
               return "create";
            when rename =>
               return "rename";
            when delete =>
               return "delete";
         end case;
      end To_String;

   begin
      JS.Write (GNATCOLL.JSON.Create (To_String (V)));
   end Write_ResourceOperationKind;

   procedure Read_FailureHandlingKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FailureHandlingKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String := JS.Read.Get;
   begin
      if Text = "abort" then
         V := abortApplying;
      elsif Text = "transactional" then
         V := transactional;
      elsif Text = "undo" then
         V := undo;
      elsif Text = "textOnlyTransactional" then
         V := textOnlyTransactional;
      else
         V := FailureHandlingKind'First;
      end if;
   end Read_FailureHandlingKind;

   procedure Write_FailureHandlingKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FailureHandlingKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_String
        (Value : FailureHandlingKind)
         return GNATCOLL.JSON.UTF8_String;

      function To_String
        (Value : FailureHandlingKind)
         return GNATCOLL.JSON.UTF8_String is
      begin
         case Value is
            when abortApplying =>
               return "abort";
            when transactional =>
               return "transactional";
            when undo =>
               return "undo";
            when textOnlyTransactional =>
               return "textOnlyTransactional";
         end case;
      end To_String;

   begin
      JS.Write (GNATCOLL.JSON.Create (To_String (V)));
   end Write_FailureHandlingKind;

   procedure Read_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceEditClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentChanges");
      Optional_Boolean'Read (S, V.documentChanges);
      JS.Key ("resourceOperations");
      Optional_ResourceOperationKindSet'Read (S, V.resourceOperations);
      JS.Key ("failureHandling");
      Optional_FailureHandlingKind'Read (S, V.failureHandling);
      JS.End_Object;
   end Read_WorkspaceEditClientCapabilities;

   procedure Write_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceEditClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentChanges");
      Optional_Boolean'Write (S, V.documentChanges);
      JS.Key ("resourceOperations");
      Optional_ResourceOperationKindSet'Write (S, V.resourceOperations);
      JS.Key ("failureHandling");
      Optional_FailureHandlingKind'Write (S, V.failureHandling);
      JS.End_Object;
   end Write_WorkspaceEditClientCapabilities;

   procedure Read_SymbolKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := SymbolKind'Val (JS.Read.Get - 1);
   end Read_SymbolKind;

   procedure Write_SymbolKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(SymbolKind'Pos (V)) + 1));
   end Write_SymbolKind;

   procedure Read_symbolKindCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out symbolKindCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      Optional_SymbolKindSet'Read (S, V.valueSet);
      JS.End_Object;
   end Read_symbolKindCapabilities;

   procedure Write_symbolKindCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : symbolKindCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      Optional_SymbolKindSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_symbolKindCapabilities;

   procedure Read_Als_Visibility
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Als_Visibility)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := Als_Visibility'Val (JS.Read.Get - 1);
   end Read_Als_Visibility;

   procedure Write_Als_Visibility
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Als_Visibility)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(Als_Visibility'Pos (V)) + 1));
   end Write_Als_Visibility;

   procedure Read_WorkspaceSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceSymbolClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Read (S, V.dynamicRegistration);
      JS.Key ("symbolKind");
      Optional_symbolKindCapabilities'Read (S, V.symbolKind);
      JS.End_Object;
   end Read_WorkspaceSymbolClientCapabilities;

   procedure Write_WorkspaceSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceSymbolClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("symbolKind");
      Optional_symbolKindCapabilities'Write (S, V.symbolKind);
      JS.End_Object;
   end Write_WorkspaceSymbolClientCapabilities;

   procedure Read_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("applyEdit");
      Optional_Boolean'Read (S, V.applyEdit);
      JS.Key ("workspaceEdit");
      WorkspaceEditClientCapabilities'Read (S, V.workspaceEdit);
      JS.Key ("didChangeConfiguration");
      DidChangeConfigurationClientCapabilities'Read (S, V.didChangeConfiguration);
      JS.Key ("didChangeWatchedFiles");
      DidChangeWatchedFilesClientCapabilities'Read (S, V.didChangeWatchedFiles);
      JS.Key ("symbol");
      Optional_WorkspaceSymbolClientCapabilities'Read (S, V.symbol);
      JS.Key ("executeCommand");
      ExecuteCommandClientCapabilities'Read (S, V.executeCommand);
      JS.Key ("workspaceFolders");
      Optional_Boolean'Read (S, V.workspaceFolders);
      JS.Key ("configuration");
      Optional_Boolean'Read (S, V.configuration);
      JS.End_Object;
   end Read_WorkspaceClientCapabilities;

   procedure Write_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("applyEdit");
      Optional_Boolean'Write (S, V.applyEdit);
      JS.Key ("workspaceEdit");
      WorkspaceEditClientCapabilities'Write (S, V.workspaceEdit);
      JS.Key ("didChangeConfiguration");
      DidChangeConfigurationClientCapabilities'Write (S, V.didChangeConfiguration);
      JS.Key ("didChangeWatchedFiles");
      DidChangeWatchedFilesClientCapabilities'Write (S, V.didChangeWatchedFiles);
      JS.Key ("symbol");
      Optional_WorkspaceSymbolClientCapabilities'Write (S, V.symbol);
      JS.Key ("executeCommand");
      ExecuteCommandClientCapabilities'Write (S, V.executeCommand);
      JS.Key ("workspaceFolders");
      Optional_Boolean'Write (S, V.workspaceFolders);
      JS.Key ("configuration");
      Optional_Boolean'Write (S, V.configuration);
      JS.End_Object;
   end Write_WorkspaceClientCapabilities;

   procedure Read_MarkupKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkupKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String := JS.Read.Get;
   begin
      if Text = "plaintext" then
         V := plaintext;
      elsif Text = "markdown" then
         V := markdown;
      else
         V := MarkupKind'First;
      end if;
   end Read_MarkupKind;

   procedure Write_MarkupKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkupKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_String
        (Value : MarkupKind)
         return GNATCOLL.JSON.UTF8_String;

      function To_String
        (Value : MarkupKind)
         return GNATCOLL.JSON.UTF8_String is
      begin
         case Value is
            when plaintext =>
               return "plaintext";
            when markdown =>
               return "markdown";
         end case;
      end To_String;

   begin
      JS.Write (GNATCOLL.JSON.Create (To_String (V)));
   end Write_MarkupKind;

   procedure Read_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkupContent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      MarkupKind'Read (S, V.kind);
      JS.Key ("value");
      LSP.Types.Read (S, V.value);
      JS.End_Object;
   end Read_MarkupContent;

   procedure Write_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkupContent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      MarkupKind'Write (S, V.kind);
      JS.Key ("value");
      LSP.Types.Write (S, V.value);
      JS.End_Object;
   end Write_MarkupContent;

   procedure Read_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SaveOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("includeText");
      Optional_Boolean'Read (S, V.includeText);
      JS.End_Object;
   end Read_SaveOptions;

   procedure Write_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SaveOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("includeText");
      Optional_Boolean'Write (S, V.includeText);
      JS.End_Object;
   end Write_SaveOptions;

   procedure Read_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Read (S, V.dynamicRegistration);
      JS.Key ("willSave");
      Optional_Boolean'Read (S, V.willSave);
      JS.Key ("willSaveWaitUntil");
      Optional_Boolean'Read (S, V.willSaveWaitUntil);
      JS.Key ("didSave");
      Optional_Boolean'Read (S, V.didSave);
      JS.End_Object;
   end Read_TextDocumentSyncClientCapabilities;

   procedure Write_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("willSave");
      Optional_Boolean'Write (S, V.willSave);
      JS.Key ("willSaveWaitUntil");
      Optional_Boolean'Write (S, V.willSaveWaitUntil);
      JS.Key ("didSave");
      Optional_Boolean'Write (S, V.didSave);
      JS.End_Object;
   end Write_TextDocumentSyncClientCapabilities;

   procedure Read_CompletionItemTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemTag)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := CompletionItemTag'Val (JS.Read.Get - 1);
   end Read_CompletionItemTag;

   procedure Write_CompletionItemTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemTag)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(CompletionItemTag'Pos (V)) + 1));
   end Write_CompletionItemTag;

   procedure Read_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemTagSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      CompletionItemTagSet'Read (S, V.valueSet);
      JS.End_Object;
   end Read_CompletionItemTagSupport;

   procedure Write_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemTagSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      CompletionItemTagSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_CompletionItemTagSupport;

   procedure Read_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out completionItemCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("snippetSupport");
      Optional_Boolean'Read (S, V.snippetSupport);
      JS.Key ("commitCharactersSupport");
      Optional_Boolean'Read (S, V.commitCharactersSupport);
      JS.Key ("documentationFormat");
      MarkupKind_Vector'Read (S, V.documentationFormat);
      JS.Key ("deprecatedSupport");
      Optional_Boolean'Read (S, V.deprecatedSupport);
      JS.Key ("preselectSupport");
      Optional_Boolean'Read (S, V.preselectSupport);
      JS.Key ("tagSupport");
      Optional_CompletionItemTagSupport'Read (S, V.tagSupport);
      JS.End_Object;
   end Read_completionItemCapability;

   procedure Write_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : completionItemCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("snippetSupport");
      Optional_Boolean'Write (S, V.snippetSupport);
      JS.Key ("commitCharactersSupport");
      Optional_Boolean'Write (S, V.commitCharactersSupport);
      JS.Key ("documentationFormat");
      MarkupKind_Vector'Write (S, V.documentationFormat);
      JS.Key ("deprecatedSupport");
      Optional_Boolean'Write (S, V.deprecatedSupport);
      JS.Key ("preselectSupport");
      Optional_Boolean'Write (S, V.preselectSupport);
      JS.Key ("tagSupport");
      Optional_CompletionItemTagSupport'Write (S, V.tagSupport);
      JS.End_Object;
   end Write_completionItemCapability;

   procedure Read_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := CompletionItemKind'Val (JS.Read.Get - 1);
   end Read_CompletionItemKind;

   procedure Write_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(CompletionItemKind'Pos (V)) + 1));
   end Write_CompletionItemKind;

   procedure Read_CompletionItemKindSetCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemKindSetCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      Optional_CompletionItemKindSet'Read (S, V.valueSet);
      JS.End_Object;
   end Read_CompletionItemKindSetCapabilities;

   procedure Write_CompletionItemKindSetCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemKindSetCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      Optional_CompletionItemKindSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_CompletionItemKindSetCapabilities;

   procedure Read_CompletionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Read (S, V.dynamicRegistration);
      JS.Key ("completionItem");
      Optional_completionItemCapability'Read (S, V.completionItem);
      JS.Key ("completionItemKind");
      Optional_CompletionItemKindSetCapabilities'Read (S, V.completionItemKind);
      JS.Key ("contextSupport");
      Optional_Boolean'Read (S, V.contextSupport);
      JS.End_Object;
   end Read_CompletionClientCapabilities;

   procedure Write_CompletionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("completionItem");
      Optional_completionItemCapability'Write (S, V.completionItem);
      JS.Key ("completionItemKind");
      Optional_CompletionItemKindSetCapabilities'Write (S, V.completionItemKind);
      JS.Key ("contextSupport");
      Optional_Boolean'Write (S, V.contextSupport);
      JS.End_Object;
   end Write_CompletionClientCapabilities;

   procedure Read_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out HoverClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Read (S, V.dynamicRegistration);
      JS.Key ("contentFormat");
      Optional_MarkupKind_Vector'Read (S, V.contentFormat);
      JS.End_Object;
   end Read_HoverClientCapabilities;

   procedure Write_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : HoverClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("contentFormat");
      Optional_MarkupKind_Vector'Write (S, V.contentFormat);
      JS.End_Object;
   end Write_HoverClientCapabilities;

   procedure Read_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out parameterInformation_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("labelOffsetSupport");
      Optional_Boolean'Read (S, V.labelOffsetSupport);
      JS.End_Object;
   end Read_parameterInformation_Capability;

   procedure Write_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : parameterInformation_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("labelOffsetSupport");
      Optional_Boolean'Write (S, V.labelOffsetSupport);
      JS.End_Object;
   end Write_parameterInformation_Capability;

   procedure Read_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out signatureInformation_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentationFormat");
      Optional_MarkupKind_Vector'Read (S, V.documentationFormat);
      JS.Key ("parameterInformation");
      Optional_parameterInformation_Capability'Read (S, V.parameterInformation);
      JS.End_Object;
   end Read_signatureInformation_Capability;

   procedure Write_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : signatureInformation_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentationFormat");
      Optional_MarkupKind_Vector'Write (S, V.documentationFormat);
      JS.Key ("parameterInformation");
      Optional_parameterInformation_Capability'Write (S, V.parameterInformation);
      JS.End_Object;
   end Write_signatureInformation_Capability;

   procedure Read_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Read (S, V.dynamicRegistration);
      JS.Key ("signatureInformation");
      Optional_signatureInformation_Capability'Read (S, V.signatureInformation);
      JS.Key ("contextSupport");
      Optional_Boolean'Read (S, V.contextSupport);
      JS.End_Object;
   end Read_SignatureHelpClientCapabilities;

   procedure Write_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("signatureInformation");
      Optional_signatureInformation_Capability'Write (S, V.signatureInformation);
      JS.Key ("contextSupport");
      Optional_Boolean'Write (S, V.contextSupport);
      JS.End_Object;
   end Write_SignatureHelpClientCapabilities;

   procedure Read_DocumentSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Read (S, V.dynamicRegistration);
      JS.Key ("symbolKind");
      Optional_symbolKindCapabilities'Read (S, V.symbolKind);
      JS.Key ("hierarchicalDocumentSymbolSupport");
      Optional_Boolean'Read (S, V.hierarchicalDocumentSymbolSupport);
      JS.End_Object;
   end Read_DocumentSymbolClientCapabilities;

   procedure Write_DocumentSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbolClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("symbolKind");
      Optional_symbolKindCapabilities'Write (S, V.symbolKind);
      JS.Key ("hierarchicalDocumentSymbolSupport");
      Optional_Boolean'Write (S, V.hierarchicalDocumentSymbolSupport);
      JS.End_Object;
   end Write_DocumentSymbolClientCapabilities;

   procedure Read_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DeclarationClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Read (S, V.dynamicRegistration);
      JS.Key ("linkSupport");
      Optional_Boolean'Read (S, V.linkSupport);
      JS.End_Object;
   end Read_DeclarationClientCapabilities;

   procedure Write_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DeclarationClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("linkSupport");
      Optional_Boolean'Write (S, V.linkSupport);
      JS.End_Object;
   end Write_DeclarationClientCapabilities;

   procedure Read_codeActionKindCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out codeActionKindCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      CodeActionKindSet'Read (S, V.valueSet);
      JS.End_Object;
   end Read_codeActionKindCapability;

   procedure Write_codeActionKindCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : codeActionKindCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      CodeActionKindSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_codeActionKindCapability;

   procedure Read_codeActionLiteralSupport_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out codeActionLiteralSupport_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("codeActionKind");
      codeActionKindCapability'Read (S, V.codeActionKind);
      JS.End_Object;
   end Read_codeActionLiteralSupport_Capability;

   procedure Write_codeActionLiteralSupport_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : codeActionLiteralSupport_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("codeActionKind");
      codeActionKindCapability'Write (S, V.codeActionKind);
      JS.End_Object;
   end Write_codeActionLiteralSupport_Capability;

   procedure Read_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Read (S, V.dynamicRegistration);
      JS.Key ("codeActionLiteralSupport");
      Optional_codeActionLiteralSupport_Capability'Read (S, V.codeActionLiteralSupport);
      JS.Key ("isPreferredSupport");
      Optional_Boolean'Read (S, V.isPreferredSupport);
      JS.End_Object;
   end Read_CodeActionClientCapabilities;

   procedure Write_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("codeActionLiteralSupport");
      Optional_codeActionLiteralSupport_Capability'Write (S, V.codeActionLiteralSupport);
      JS.Key ("isPreferredSupport");
      Optional_Boolean'Write (S, V.isPreferredSupport);
      JS.End_Object;
   end Write_CodeActionClientCapabilities;

   procedure Read_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentLinkClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Read (S, V.dynamicRegistration);
      JS.Key ("tooltipSupport");
      Optional_Boolean'Read (S, V.tooltipSupport);
      JS.End_Object;
   end Read_DocumentLinkClientCapabilities;

   procedure Write_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentLinkClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("tooltipSupport");
      Optional_Boolean'Write (S, V.tooltipSupport);
      JS.End_Object;
   end Write_DocumentLinkClientCapabilities;

   procedure Read_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Read (S, V.dynamicRegistration);
      JS.Key ("prepareSupport");
      Optional_Boolean'Read (S, V.prepareSupport);
      JS.End_Object;
   end Read_RenameClientCapabilities;

   procedure Write_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("prepareSupport");
      Optional_Boolean'Write (S, V.prepareSupport);
      JS.End_Object;
   end Write_RenameClientCapabilities;

   procedure Read_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticTagSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      DiagnosticTagSet'Read (S, V.valueSet);
      JS.End_Object;
   end Read_DiagnosticTagSupport;

   procedure Write_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticTagSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      DiagnosticTagSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_DiagnosticTagSupport;

   procedure Read_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PublishDiagnosticsClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("relatedInformation");
      Optional_Boolean'Read (S, V.relatedInformation);
      JS.Key ("tagSupport");
      Optional_DiagnosticTagSupport'Read (S, V.tagSupport);
      JS.Key ("versionSupport");
      Optional_Boolean'Read (S, V.versionSupport);
      JS.End_Object;
   end Read_PublishDiagnosticsClientCapabilities;

   procedure Write_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PublishDiagnosticsClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("relatedInformation");
      Optional_Boolean'Write (S, V.relatedInformation);
      JS.Key ("tagSupport");
      Optional_DiagnosticTagSupport'Write (S, V.tagSupport);
      JS.Key ("versionSupport");
      Optional_Boolean'Write (S, V.versionSupport);
      JS.End_Object;
   end Write_PublishDiagnosticsClientCapabilities;

   procedure Read_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FoldingRangeClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Read (S, V.dynamicRegistration);
      JS.Key ("rangeLimit");
      Optional_Number'Read (S, V.rangeLimit);
      JS.Key ("lineFoldingOnly");
      Optional_Boolean'Read (S, V.lineFoldingOnly);
      JS.End_Object;
   end Read_FoldingRangeClientCapabilities;

   procedure Write_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRangeClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("rangeLimit");
      Optional_Number'Write (S, V.rangeLimit);
      JS.Key ("lineFoldingOnly");
      Optional_Boolean'Write (S, V.lineFoldingOnly);
      JS.End_Object;
   end Write_FoldingRangeClientCapabilities;

   procedure Read_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("synchronization");
      TextDocumentSyncClientCapabilities'Read (S, V.synchronization);
      JS.Key ("completion");
      CompletionClientCapabilities'Read (S, V.completion);
      JS.Key ("hover");
      Optional_HoverClientCapabilities'Read (S, V.hover);
      JS.Key ("signatureHelp");
      Optional_SignatureHelpClientCapabilities'Read (S, V.signatureHelp);
      JS.Key ("declaration");
      Optional_DeclarationClientCapabilities'Read (S, V.declaration);
      JS.Key ("definition");
      Optional_DefinitionClientCapabilities'Read (S, V.definition);
      JS.Key ("typeDefinition");
      Optional_TypeDefinitionClientCapabilities'Read (S, V.typeDefinition);
      JS.Key ("implementation");
      Optional_ImplementationClientCapabilities'Read (S, V.implementation);
      JS.Key ("references");
      ReferenceClientCapabilities'Read (S, V.references);
      JS.Key ("documentHighlight");
      DocumentHighlightClientCapabilities'Read (S, V.documentHighlight);
      JS.Key ("documentSymbol");
      Optional_DocumentSymbolClientCapabilities'Read (S, V.documentSymbol);
      JS.Key ("codeAction");
      Optional_CodeActionClientCapabilities'Read (S, V.codeAction);
      JS.Key ("codeLens");
      CodeLensClientCapabilities'Read (S, V.codeLens);
      JS.Key ("documentLink");
      Optional_DocumentLinkClientCapabilities'Read (S, V.documentLink);
      JS.Key ("colorProvider");
      DocumentColorClientCapabilities'Read (S, V.colorProvider);
      JS.Key ("formatting");
      DocumentFormattingClientCapabilities'Read (S, V.formatting);
      JS.Key ("rangeFormatting");
      DocumentRangeFormattingClientCapabilities'Read (S, V.rangeFormatting);
      JS.Key ("onTypeFormatting");
      DocumentOnTypeFormattingClientCapabilities'Read (S, V.onTypeFormatting);
      JS.Key ("rename");
      Optional_RenameClientCapabilities'Read (S, V.rename);
      JS.Key ("publishDiagnostics");
      Optional_PublishDiagnosticsClientCapabilities'Read (S, V.publishDiagnostics);
      JS.Key ("foldingRange");
      Optional_FoldingRangeClientCapabilities'Read (S, V.foldingRange);
      JS.Key ("selectionRange");
      SelectionRangeClientCapabilities'Read (S, V.selectionRange);
      JS.End_Object;
   end Read_TextDocumentClientCapabilities;

   procedure Write_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("synchronization");
      TextDocumentSyncClientCapabilities'Write (S, V.synchronization);
      JS.Key ("completion");
      CompletionClientCapabilities'Write (S, V.completion);
      JS.Key ("hover");
      Optional_HoverClientCapabilities'Write (S, V.hover);
      JS.Key ("signatureHelp");
      Optional_SignatureHelpClientCapabilities'Write (S, V.signatureHelp);
      JS.Key ("declaration");
      Optional_DeclarationClientCapabilities'Write (S, V.declaration);
      JS.Key ("definition");
      Optional_DefinitionClientCapabilities'Write (S, V.definition);
      JS.Key ("typeDefinition");
      Optional_TypeDefinitionClientCapabilities'Write (S, V.typeDefinition);
      JS.Key ("implementation");
      Optional_ImplementationClientCapabilities'Write (S, V.implementation);
      JS.Key ("references");
      ReferenceClientCapabilities'Write (S, V.references);
      JS.Key ("documentHighlight");
      DocumentHighlightClientCapabilities'Write (S, V.documentHighlight);
      JS.Key ("documentSymbol");
      Optional_DocumentSymbolClientCapabilities'Write (S, V.documentSymbol);
      JS.Key ("codeAction");
      Optional_CodeActionClientCapabilities'Write (S, V.codeAction);
      JS.Key ("codeLens");
      CodeLensClientCapabilities'Write (S, V.codeLens);
      JS.Key ("documentLink");
      Optional_DocumentLinkClientCapabilities'Write (S, V.documentLink);
      JS.Key ("colorProvider");
      DocumentColorClientCapabilities'Write (S, V.colorProvider);
      JS.Key ("formatting");
      DocumentFormattingClientCapabilities'Write (S, V.formatting);
      JS.Key ("rangeFormatting");
      DocumentRangeFormattingClientCapabilities'Write (S, V.rangeFormatting);
      JS.Key ("onTypeFormatting");
      DocumentOnTypeFormattingClientCapabilities'Write (S, V.onTypeFormatting);
      JS.Key ("rename");
      Optional_RenameClientCapabilities'Write (S, V.rename);
      JS.Key ("publishDiagnostics");
      Optional_PublishDiagnosticsClientCapabilities'Write (S, V.publishDiagnostics);
      JS.Key ("foldingRange");
      Optional_FoldingRangeClientCapabilities'Write (S, V.foldingRange);
      JS.Key ("selectionRange");
      SelectionRangeClientCapabilities'Write (S, V.selectionRange);
      JS.End_Object;
   end Write_TextDocumentClientCapabilities;

   procedure Read_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WindowClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Read (S, V.workDoneProgress);
      JS.End_Object;
   end Read_WindowClientCapabilities;

   procedure Write_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WindowClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.End_Object;
   end Write_WindowClientCapabilities;

   procedure Read_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workspace");
      WorkspaceClientCapabilities'Read (S, V.workspace);
      JS.Key ("textDocument");
      TextDocumentClientCapabilities'Read (S, V.textDocument);
      JS.Key ("window");
      Optional_WindowClientCapabilities'Read (S, V.window);
      JS.End_Object;
   end Read_ClientCapabilities;

   procedure Write_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workspace");
      WorkspaceClientCapabilities'Write (S, V.workspace);
      JS.Key ("textDocument");
      TextDocumentClientCapabilities'Write (S, V.textDocument);
      JS.Key ("window");
      Optional_WindowClientCapabilities'Write (S, V.window);
      JS.End_Object;
   end Write_ClientCapabilities;

   procedure Read_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFolder)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Read (S, V.uri);
      JS.Key ("name");
      LSP.Types.Read (S, V.name);
      JS.End_Object;
   end Read_WorkspaceFolder;

   procedure Write_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFolder)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write (S, V.uri);
      JS.Key ("name");
      LSP.Types.Write (S, V.name);
      JS.End_Object;
   end Write_WorkspaceFolder;

   procedure Read_WorkDoneProgressCreateParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressCreateParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("token");
      ProgressToken'Read (S, V.token);
      JS.End_Object;
   end Read_WorkDoneProgressCreateParams;

   procedure Write_WorkDoneProgressCreateParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressCreateParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("token");
      ProgressToken'Write (S, V.token);
      JS.End_Object;
   end Write_WorkDoneProgressCreateParams;

   procedure Read_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ProgramInfo)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("name");
      LSP.Types.Read (S, V.name);
      JS.Key ("version");
      Optional_String'Read (S, V.version);
      JS.End_Object;
   end Read_ProgramInfo;

   procedure Write_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ProgramInfo)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("name");
      LSP.Types.Write (S, V.name);
      JS.Key ("version");
      Optional_String'Write (S, V.version);
      JS.End_Object;
   end Write_ProgramInfo;

   procedure Read_Trace_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Trace_Kind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String := JS.Read.Get;
   begin
      if Text = "off" then
         V := off;
      elsif Text = "messages_trace" then
         V := messages_trace;
      elsif Text = "verbose" then
         V := verbose;
      else
         V := Trace_Kind'First;
      end if;
   end Read_Trace_Kind;

   procedure Write_Trace_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Trace_Kind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_String
        (Value : Trace_Kind)
         return GNATCOLL.JSON.UTF8_String;

      function To_String
        (Value : Trace_Kind)
         return GNATCOLL.JSON.UTF8_String is
      begin
         case Value is
            when off =>
               return "off";
            when messages_trace =>
               return "messages_trace";
            when verbose =>
               return "verbose";
         end case;
      end To_String;

   begin
      JS.Write (GNATCOLL.JSON.Create (To_String (V)));
   end Write_Trace_Kind;

   procedure Read_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := TextDocumentSyncKind'Val (JS.Read.Get - 0);
   end Read_TextDocumentSyncKind;

   procedure Write_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(TextDocumentSyncKind'Pos (V)) + 0));
   end Write_TextDocumentSyncKind;

   procedure Read_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("openClose");
      Optional_Boolean'Read (S, V.openClose);
      JS.Key ("change");
      Optional_TextDocumentSyncKind'Read (S, V.change);
      JS.Key ("willSave");
      Optional_Boolean'Read (S, V.willSave);
      JS.Key ("willSaveWaitUntil");
      Optional_Boolean'Read (S, V.willSaveWaitUntil);
      JS.Key ("save");
      Optional_SaveOptions'Read (S, V.save);
      JS.End_Object;
   end Read_TextDocumentSyncOptions;

   procedure Write_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("openClose");
      Optional_Boolean'Write (S, V.openClose);
      JS.Key ("change");
      Optional_TextDocumentSyncKind'Write (S, V.change);
      JS.Key ("willSave");
      Optional_Boolean'Write (S, V.willSave);
      JS.Key ("willSaveWaitUntil");
      Optional_Boolean'Write (S, V.willSaveWaitUntil);
      JS.Key ("save");
      Optional_SaveOptions'Write (S, V.save);
      JS.End_Object;
   end Write_TextDocumentSyncOptions;

   procedure Read_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Read (S, V.workDoneProgress);
      JS.Key ("triggerCharacters");
      Optional_LSP_String_Vector'Read (S, V.triggerCharacters);
      JS.Key ("allCommitCharacters");
      Optional_LSP_String_Vector'Read (S, V.allCommitCharacters);
      JS.Key ("resolveProvider");
      LSP.Types.Optional_Boolean'Read (S, V.resolveProvider);
      JS.End_Object;
   end Read_CompletionOptions;

   procedure Write_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("triggerCharacters");
      Optional_LSP_String_Vector'Write (S, V.triggerCharacters);
      JS.Key ("allCommitCharacters");
      Optional_LSP_String_Vector'Write (S, V.allCommitCharacters);
      JS.Key ("resolveProvider");
      LSP.Types.Optional_Boolean'Write (S, V.resolveProvider);
      JS.End_Object;
   end Write_CompletionOptions;

   procedure Read_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Read (S, V.workDoneProgress);
      JS.Key ("triggerCharacters");
      Optional_LSP_String_Vector'Read (S, V.triggerCharacters);
      JS.Key ("retriggerCharacters");
      Optional_LSP_String_Vector'Read (S, V.retriggerCharacters);
      JS.End_Object;
   end Read_SignatureHelpOptions;

   procedure Write_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("triggerCharacters");
      Optional_LSP_String_Vector'Write (S, V.triggerCharacters);
      JS.Key ("retriggerCharacters");
      Optional_LSP_String_Vector'Write (S, V.retriggerCharacters);
      JS.End_Object;
   end Write_SignatureHelpOptions;

   procedure Read_TSW_RegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TSW_RegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Read (S, V.workDoneProgress);
      JS.Key ("id");
      Optional_String'Read (S, V.id);
      JS.Key ("documentSelector");
      LSP.Messages.DocumentSelector'Read (S, V.documentSelector);
      JS.End_Object;
   end Read_TSW_RegistrationOptions;

   procedure Write_TSW_RegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TSW_RegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("id");
      Optional_String'Write (S, V.id);
      JS.Key ("documentSelector");
      LSP.Messages.DocumentSelector'Write (S, V.documentSelector);
      JS.End_Object;
   end Write_TSW_RegistrationOptions;

   procedure Read_CodeActionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Read (S, V.workDoneProgress);
      JS.Key ("codeActionKinds");
      Optional_CodeActionKindSet'Read (S, V.codeActionKinds);
      JS.End_Object;
   end Read_CodeActionOptions;

   procedure Write_CodeActionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("codeActionKinds");
      Optional_CodeActionKindSet'Write (S, V.codeActionKinds);
      JS.End_Object;
   end Write_CodeActionOptions;

   procedure Read_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeLensOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Read (S, V.workDoneProgress);
      JS.Key ("resolveProvider");
      LSP.Types.Optional_Boolean'Read (S, V.resolveProvider);
      JS.End_Object;
   end Read_CodeLensOptions;

   procedure Write_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeLensOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("resolveProvider");
      LSP.Types.Optional_Boolean'Write (S, V.resolveProvider);
      JS.End_Object;
   end Write_CodeLensOptions;

   procedure Read_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Read (S, V.workDoneProgress);
      JS.Key ("firstTriggerCharacter");
      LSP.Types.LSP_String'Read (S, V.firstTriggerCharacter);
      JS.Key ("moreTriggerCharacter");
      Optional_LSP_String_Vector'Read (S, V.moreTriggerCharacter);
      JS.End_Object;
   end Read_DocumentOnTypeFormattingOptions;

   procedure Write_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("firstTriggerCharacter");
      LSP.Types.LSP_String'Write (S, V.firstTriggerCharacter);
      JS.Key ("moreTriggerCharacter");
      Optional_LSP_String_Vector'Write (S, V.moreTriggerCharacter);
      JS.End_Object;
   end Write_DocumentOnTypeFormattingOptions;

   procedure Read_RenameOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Read (S, V.workDoneProgress);
      JS.Key ("prepareProvider");
      LSP.Types.Optional_Boolean'Read (S, V.prepareProvider);
      JS.End_Object;
   end Read_RenameOptions;

   procedure Write_RenameOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("prepareProvider");
      LSP.Types.Optional_Boolean'Write (S, V.prepareProvider);
      JS.End_Object;
   end Write_RenameOptions;

   procedure Read_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentLinkOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Read (S, V.workDoneProgress);
      JS.Key ("resolveProvider");
      LSP.Types.Optional_Boolean'Read (S, V.resolveProvider);
      JS.End_Object;
   end Read_DocumentLinkOptions;

   procedure Write_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentLinkOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("resolveProvider");
      LSP.Types.Optional_Boolean'Write (S, V.resolveProvider);
      JS.End_Object;
   end Write_DocumentLinkOptions;

   procedure Read_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Read (S, V.workDoneProgress);
      JS.Key ("commands");
      LSP.Types.LSP_String_Vector'Read (S, V.commands);
      JS.End_Object;
   end Read_ExecuteCommandOptions;

   procedure Write_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("commands");
      LSP.Types.LSP_String_Vector'Write (S, V.commands);
      JS.End_Object;
   end Write_ExecuteCommandOptions;

   procedure Read_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFoldersServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("supported");
      Optional_Boolean'Read (S, V.supported);
      JS.Key ("changeNotifications");
      Optional_Boolean_Or_String'Read (S, V.changeNotifications);
      JS.End_Object;
   end Read_WorkspaceFoldersServerCapabilities;

   procedure Write_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFoldersServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("supported");
      Optional_Boolean'Write (S, V.supported);
      JS.Key ("changeNotifications");
      Optional_Boolean_Or_String'Write (S, V.changeNotifications);
      JS.End_Object;
   end Write_WorkspaceFoldersServerCapabilities;

   procedure Read_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out workspace_Options)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workspaceFolders");
      Optional_WorkspaceFoldersServerCapabilities'Read (S, V.workspaceFolders);
      JS.End_Object;
   end Read_workspace_Options;

   procedure Write_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : workspace_Options)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workspaceFolders");
      Optional_WorkspaceFoldersServerCapabilities'Write (S, V.workspaceFolders);
      JS.End_Object;
   end Write_workspace_Options;

   procedure Read_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocumentSync");
      Optional_TextDocumentSyncOptions'Read (S, V.textDocumentSync);
      JS.Key ("completionProvider");
      Optional_CompletionOptions'Read (S, V.completionProvider);
      JS.Key ("hoverProvider");
      HoverOptions'Read (S, V.hoverProvider);
      JS.Key ("signatureHelpProvider");
      Optional_SignatureHelpOptions'Read (S, V.signatureHelpProvider);
      JS.Key ("declarationProvider");
      DeclarationOptions'Read (S, V.declarationProvider);
      JS.Key ("definitionProvider");
      DefinitionOptions'Read (S, V.definitionProvider);
      JS.Key ("typeDefinitionProvider");
      TypeDefinitionOptions'Read (S, V.typeDefinitionProvider);
      JS.Key ("implementationProvider");
      ImplementationOptions'Read (S, V.implementationProvider);
      JS.Key ("referencesProvider");
      ReferenceOptions'Read (S, V.referencesProvider);
      JS.Key ("documentHighlightProvider");
      DocumentHighlightOptions'Read (S, V.documentHighlightProvider);
      JS.Key ("documentSymbolProvider");
      DocumentSymbolOptions'Read (S, V.documentSymbolProvider);
      JS.Key ("codeActionProvider");
      Optional_CodeActionOptions'Read (S, V.codeActionProvider);
      JS.Key ("codeLensProvider");
      Optional_CodeLensOptions'Read (S, V.codeLensProvider);
      JS.Key ("documentLinkProvider");
      Optional_DocumentLinkOptions'Read (S, V.documentLinkProvider);
      JS.Key ("colorProvider");
      DocumentColorOptions'Read (S, V.colorProvider);
      JS.Key ("documentFormattingProvider");
      DocumentFormattingOptions'Read (S, V.documentFormattingProvider);
      JS.Key ("documentRangeFormattingProvider");
      DocumentRangeFormattingOptions'Read (S, V.documentRangeFormattingProvider);
      JS.Key ("documentOnTypeFormattingProvider");
      Optional_DocumentOnTypeFormattingOptions'Read (S, V.documentOnTypeFormattingProvider);
      JS.Key ("renameProvider");
      Optional_RenameOptions'Read (S, V.renameProvider);
      JS.Key ("foldingRangeProvider");
      FoldingRangeOptions'Read (S, V.foldingRangeProvider);
      JS.Key ("executeCommandProvider");
      Optional_ExecuteCommandOptions'Read (S, V.executeCommandProvider);
      JS.Key ("selectionRangeProvider");
      SelectionRangeOptions'Read (S, V.selectionRangeProvider);
      JS.Key ("workspaceSymbolProvider");
      WorkspaceSymbolOptions'Read (S, V.workspaceSymbolProvider);
      JS.Key ("workspace");
      Optional_workspace_Options'Read (S, V.workspace);
      JS.Key ("alsCalledByProvider");
      Optional_Boolean'Read (S, V.alsCalledByProvider);
      JS.Key ("alsReferenceKinds");
      Optional_AlsReferenceKind_Set'Read (S, V.alsReferenceKinds);
      JS.End_Object;
   end Read_ServerCapabilities;

   procedure Write_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocumentSync");
      Optional_TextDocumentSyncOptions'Write (S, V.textDocumentSync);
      JS.Key ("completionProvider");
      Optional_CompletionOptions'Write (S, V.completionProvider);
      JS.Key ("hoverProvider");
      HoverOptions'Write (S, V.hoverProvider);
      JS.Key ("signatureHelpProvider");
      Optional_SignatureHelpOptions'Write (S, V.signatureHelpProvider);
      JS.Key ("declarationProvider");
      DeclarationOptions'Write (S, V.declarationProvider);
      JS.Key ("definitionProvider");
      DefinitionOptions'Write (S, V.definitionProvider);
      JS.Key ("typeDefinitionProvider");
      TypeDefinitionOptions'Write (S, V.typeDefinitionProvider);
      JS.Key ("implementationProvider");
      ImplementationOptions'Write (S, V.implementationProvider);
      JS.Key ("referencesProvider");
      ReferenceOptions'Write (S, V.referencesProvider);
      JS.Key ("documentHighlightProvider");
      DocumentHighlightOptions'Write (S, V.documentHighlightProvider);
      JS.Key ("documentSymbolProvider");
      DocumentSymbolOptions'Write (S, V.documentSymbolProvider);
      JS.Key ("codeActionProvider");
      Optional_CodeActionOptions'Write (S, V.codeActionProvider);
      JS.Key ("codeLensProvider");
      Optional_CodeLensOptions'Write (S, V.codeLensProvider);
      JS.Key ("documentLinkProvider");
      Optional_DocumentLinkOptions'Write (S, V.documentLinkProvider);
      JS.Key ("colorProvider");
      DocumentColorOptions'Write (S, V.colorProvider);
      JS.Key ("documentFormattingProvider");
      DocumentFormattingOptions'Write (S, V.documentFormattingProvider);
      JS.Key ("documentRangeFormattingProvider");
      DocumentRangeFormattingOptions'Write (S, V.documentRangeFormattingProvider);
      JS.Key ("documentOnTypeFormattingProvider");
      Optional_DocumentOnTypeFormattingOptions'Write (S, V.documentOnTypeFormattingProvider);
      JS.Key ("renameProvider");
      Optional_RenameOptions'Write (S, V.renameProvider);
      JS.Key ("foldingRangeProvider");
      FoldingRangeOptions'Write (S, V.foldingRangeProvider);
      JS.Key ("executeCommandProvider");
      Optional_ExecuteCommandOptions'Write (S, V.executeCommandProvider);
      JS.Key ("selectionRangeProvider");
      SelectionRangeOptions'Write (S, V.selectionRangeProvider);
      JS.Key ("workspaceSymbolProvider");
      WorkspaceSymbolOptions'Write (S, V.workspaceSymbolProvider);
      JS.Key ("workspace");
      Optional_workspace_Options'Write (S, V.workspace);
      JS.Key ("alsCalledByProvider");
      Optional_Boolean'Write (S, V.alsCalledByProvider);
      JS.Key ("alsReferenceKinds");
      Optional_AlsReferenceKind_Set'Write (S, V.alsReferenceKinds);
      JS.End_Object;
   end Write_ServerCapabilities;

   procedure Read_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("capabilities");
      ServerCapabilities'Read (S, V.capabilities);
      JS.Key ("serverInfo");
      Optional_ProgramInfo'Read (S, V.serverInfo);
      JS.End_Object;
   end Read_InitializeResult;

   procedure Write_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("capabilities");
      ServerCapabilities'Write (S, V.capabilities);
      JS.Key ("serverInfo");
      Optional_ProgramInfo'Write (S, V.serverInfo);
      JS.End_Object;
   end Write_InitializeResult;

   procedure Read_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializedParams)
   is
      pragma Unreferenced (V);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.End_Object;
   end Read_InitializedParams;

   procedure Write_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializedParams)
   is
      pragma Unreferenced (V);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.End_Object;
   end Write_InitializedParams;

   procedure Read_MessageType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MessageType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := MessageType'Val (JS.Read.Get - 1);
   end Read_MessageType;

   procedure Write_MessageType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MessageType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(MessageType'Pos (V)) + 1));
   end Write_MessageType;

   procedure Read_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Read (S, V.the_type);
      JS.Key ("message");
      LSP.Types.Read (S, V.message);
      JS.End_Object;
   end Read_ShowMessageParams;

   procedure Write_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Write (S, V.the_type);
      JS.Key ("message");
      LSP.Types.Write (S, V.message);
      JS.End_Object;
   end Write_ShowMessageParams;

   procedure Read_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageRequestParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Read (S, V.the_type);
      JS.Key ("message");
      LSP.Types.Read (S, V.message);
      JS.Key ("actions");
      MessageActionItem_Vector'Read (S, V.actions);
      JS.End_Object;
   end Read_ShowMessageRequestParams;

   procedure Write_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageRequestParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Write (S, V.the_type);
      JS.Key ("message");
      LSP.Types.Write (S, V.message);
      JS.Key ("actions");
      MessageActionItem_Vector'Write (S, V.actions);
      JS.End_Object;
   end Write_ShowMessageRequestParams;

   procedure Read_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LogMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Read (S, V.the_type);
      JS.Key ("message");
      LSP.Types.Read (S, V.message);
      JS.End_Object;
   end Read_LogMessageParams;

   procedure Write_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LogMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Write (S, V.the_type);
      JS.Key ("message");
      LSP.Types.Write (S, V.message);
      JS.End_Object;
   end Write_LogMessageParams;

   procedure Read_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("settings");
      LSP.Types.LSP_Any'Read (S, V.settings);
      JS.End_Object;
   end Read_DidChangeConfigurationParams;

   procedure Write_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("settings");
      LSP.Types.LSP_Any'Write (S, V.settings);
      JS.End_Object;
   end Write_DidChangeConfigurationParams;

   procedure Read_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidOpenTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentItem'Read (S, V.textDocument);
      JS.End_Object;
   end Read_DidOpenTextDocumentParams;

   procedure Write_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidOpenTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentItem'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DidOpenTextDocumentParams;

   procedure Read_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentContentChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Optional_Span'Read (S, V.span);
      JS.Key ("rangeLength");
      LSP.Types.Optional_Number'Read (S, V.rangeLength);
      JS.Key ("text");
      LSP.Types.Read (S, V.text);
      JS.End_Object;
   end Read_TextDocumentContentChangeEvent;

   procedure Write_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentContentChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Optional_Span'Write (S, V.span);
      JS.Key ("rangeLength");
      LSP.Types.Optional_Number'Write (S, V.rangeLength);
      JS.Key ("text");
      LSP.Types.Write (S, V.text);
      JS.End_Object;
   end Write_TextDocumentContentChangeEvent;

   procedure Read_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("contentChanges");
      TextDocumentContentChangeEvent_Vector'Read (S, V.contentChanges);
      JS.End_Object;
   end Read_DidChangeTextDocumentParams;

   procedure Write_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("contentChanges");
      TextDocumentContentChangeEvent_Vector'Write (S, V.contentChanges);
      JS.End_Object;
   end Write_DidChangeTextDocumentParams;

   procedure Read_TextDocumentSaveReason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSaveReason)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := TextDocumentSaveReason'Val (JS.Read.Get - 1);
   end Read_TextDocumentSaveReason;

   procedure Write_TextDocumentSaveReason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSaveReason)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(TextDocumentSaveReason'Pos (V)) + 1));
   end Write_TextDocumentSaveReason;

   procedure Read_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidSaveTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("text");
      Optional_String'Read (S, V.text);
      JS.End_Object;
   end Read_DidSaveTextDocumentParams;

   procedure Write_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidSaveTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("text");
      Optional_String'Write (S, V.text);
      JS.End_Object;
   end Write_DidSaveTextDocumentParams;

   procedure Read_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidCloseTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.End_Object;
   end Read_DidCloseTextDocumentParams;

   procedure Write_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidCloseTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DidCloseTextDocumentParams;

   procedure Read_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileChangeType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := FileChangeType'Val (JS.Read.Get - 1);
   end Read_FileChangeType;

   procedure Write_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileChangeType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(FileChangeType'Pos (V)) + 1));
   end Write_FileChangeType;

   procedure Read_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PublishDiagnosticsParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Read (S, V.uri);
      JS.Key ("version");
      Optional_Number'Read (S, V.version);
      JS.Key ("diagnostics");
      Diagnostic_Vector'Read (S, V.diagnostics);
      JS.End_Object;
   end Read_PublishDiagnosticsParams;

   procedure Write_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PublishDiagnosticsParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write (S, V.uri);
      JS.Key ("version");
      Optional_Number'Write (S, V.version);
      JS.Key ("diagnostics");
      Diagnostic_Vector'Write (S, V.diagnostics);
      JS.End_Object;
   end Write_PublishDiagnosticsParams;

   procedure Read_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InsertTextFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := InsertTextFormat'Val (JS.Read.Get - 1);
   end Read_InsertTextFormat;

   procedure Write_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InsertTextFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(InsertTextFormat'Pos (V)) + 1));
   end Write_InsertTextFormat;

   procedure Read_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      LSP.Types.Read (S, V.label);
      JS.Key ("kind");
      Optional_CompletionItemKind'Read (S, V.kind);
      JS.Key ("tags");
      Optional_CompletionItemTagSet'Read (S, V.tags);
      JS.Key ("detail");
      Optional_String'Read (S, V.detail);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Read (S, V.documentation);
      JS.Key ("deprecated");
      Optional_Boolean'Read (S, V.deprecated);
      JS.Key ("preselect");
      Optional_Boolean'Read (S, V.preselect);
      JS.Key ("sortText");
      Optional_String'Read (S, V.sortText);
      JS.Key ("filterText");
      Optional_String'Read (S, V.filterText);
      JS.Key ("insertText");
      Optional_String'Read (S, V.insertText);
      JS.Key ("insertTextFormat");
      Optional_InsertTextFormat'Read (S, V.insertTextFormat);
      JS.Key ("textEdit");
      Optional_TextEdit'Read (S, V.textEdit);
      JS.Key ("additionalTextEdits");
      TextEdit_Vector'Read (S, V.additionalTextEdits);
      JS.Key ("commitCharacters");
      Optional_LSP_String_Vector'Read (S, V.commitCharacters);
      JS.Key ("command");
      Optional_Command'Read (S, V.command);
      JS.End_Object;
   end Read_CompletionItem;

   procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      LSP.Types.Write (S, V.label);
      JS.Key ("kind");
      Optional_CompletionItemKind'Write (S, V.kind);
      JS.Key ("tags");
      Optional_CompletionItemTagSet'Write (S, V.tags);
      JS.Key ("detail");
      Optional_String'Write (S, V.detail);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Write (S, V.documentation);
      JS.Key ("deprecated");
      Optional_Boolean'Write (S, V.deprecated);
      JS.Key ("preselect");
      Optional_Boolean'Write (S, V.preselect);
      JS.Key ("sortText");
      Optional_String'Write (S, V.sortText);
      JS.Key ("filterText");
      Optional_String'Write (S, V.filterText);
      JS.Key ("insertText");
      Optional_String'Write (S, V.insertText);
      JS.Key ("insertTextFormat");
      Optional_InsertTextFormat'Write (S, V.insertTextFormat);
      JS.Key ("textEdit");
      Optional_TextEdit'Write (S, V.textEdit);
      JS.Key ("additionalTextEdits");
      TextEdit_Vector'Write (S, V.additionalTextEdits);
      JS.Key ("commitCharacters");
      Optional_LSP_String_Vector'Write (S, V.commitCharacters);
      JS.Key ("command");
      Optional_Command'Write (S, V.command);
      JS.End_Object;
   end Write_CompletionItem;

   procedure Read_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionList)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Boolean (JS, +"isIncomplete", V.isIncomplete);
      JS.Key ("items");
      CompletionItem_Vector'Read (S, V.items);
      JS.End_Object;
   end Read_CompletionList;

   procedure Write_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionList)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Boolean (JS, +"isIncomplete", V.isIncomplete);
      JS.Key ("items");
      CompletionItem_Vector'Write (S, V.items);
      JS.End_Object;
   end Write_CompletionList;

   procedure Read_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Hover)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("contents");
      MarkupContent_Or_MarkedString_Vector'Read (S, V.contents);
      JS.Key ("range");
      Optional_Span'Read (S, V.Span);
      JS.End_Object;
   end Read_Hover;

   procedure Write_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Hover)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("contents");
      MarkupContent_Or_MarkedString_Vector'Write (S, V.contents);
      JS.Key ("range");
      Optional_Span'Write (S, V.Span);
      JS.End_Object;
   end Write_Hover;

   procedure Read_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ParameterInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      Parameter_Label'Read (S, V.label);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Read (S, V.documentation);
      JS.End_Object;
   end Read_ParameterInformation;

   procedure Write_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ParameterInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      Parameter_Label'Write (S, V.label);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Write (S, V.documentation);
      JS.End_Object;
   end Write_ParameterInformation;

   procedure Read_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      LSP.Types.Read (S, V.label);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Read (S, V.documentation);
      JS.Key ("parameters");
      ParameterInformation_Vector'Read (S, V.parameters);
      JS.End_Object;
   end Read_SignatureInformation;

   procedure Write_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      LSP.Types.Write (S, V.label);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Write (S, V.documentation);
      JS.Key ("parameters");
      ParameterInformation_Vector'Write (S, V.parameters);
      JS.End_Object;
   end Write_SignatureInformation;

   procedure Read_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelp)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("signatures");
      SignatureInformation_Vector'Read (S, V.signatures);
      JS.Key ("activeSignature");
      Optional_Number'Read (S, V.activeSignature);
      JS.Key ("activeParameter");
      Optional_Number'Read (S, V.activeParameter);
      JS.End_Object;
   end Read_SignatureHelp;

   procedure Write_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelp)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("signatures");
      SignatureInformation_Vector'Write (S, V.signatures);
      JS.Key ("activeSignature");
      Optional_Number'Write (S, V.activeSignature);
      JS.Key ("activeParameter");
      Optional_Number'Write (S, V.activeParameter);
      JS.End_Object;
   end Write_SignatureHelp;

   procedure Read_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ReferenceContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Boolean (JS, +"includeDeclaration", V.includeDeclaration);
      JS.End_Object;
   end Read_ReferenceContext;

   procedure Write_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReferenceContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Boolean (JS, +"includeDeclaration", V.includeDeclaration);
      JS.End_Object;
   end Write_ReferenceContext;

   procedure Read_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ReferenceParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Read (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Read (S, V.partialResultToken);
      JS.Key ("context");
      ReferenceContext'Read (S, V.context);
      JS.End_Object;
   end Read_ReferenceParams;

   procedure Write_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReferenceParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("context");
      ReferenceContext'Write (S, V.context);
      JS.End_Object;
   end Write_ReferenceParams;

   procedure Read_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentHighlightKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := DocumentHighlightKind'Val (JS.Read.Get - 1);
   end Read_DocumentHighlightKind;

   procedure Write_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlightKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(DocumentHighlightKind'Pos (V)) + 1));
   end Write_DocumentHighlightKind;

   procedure Read_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentHighlight)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Read (S, V.span);
      JS.Key ("kind");
      Optional_DocumentHighlightKind'Read (S, V.kind);
      JS.End_Object;
   end Read_DocumentHighlight;

   procedure Write_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlight)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("kind");
      Optional_DocumentHighlightKind'Write (S, V.kind);
      JS.End_Object;
   end Write_DocumentHighlight;

   procedure Read_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Read (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.End_Object;
   end Read_DocumentSymbolParams;

   procedure Write_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DocumentSymbolParams;

   procedure Read_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("name");
      LSP.Types.Read (S, V.name);
      JS.Key ("kind");
      SymbolKind'Read (S, V.kind);
      JS.Key ("alsIsAdaProcedure");
      Optional_Boolean'Read (S, V.alsIsAdaProcedure);
      JS.Key ("deprecated");
      Optional_Boolean'Read (S, V.deprecated);
      JS.Key ("location");
      LSP.Messages.Location'Read (S, V.location);
      JS.Key ("containerName");
      Optional_String'Read (S, V.containerName);
      JS.End_Object;
   end Read_SymbolInformation;

   procedure Write_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("name");
      LSP.Types.Write (S, V.name);
      JS.Key ("kind");
      SymbolKind'Write (S, V.kind);
      JS.Key ("alsIsAdaProcedure");
      Optional_Boolean'Write (S, V.alsIsAdaProcedure);
      JS.Key ("deprecated");
      Optional_Boolean'Write (S, V.deprecated);
      JS.Key ("location");
      LSP.Messages.Location'Write (S, V.location);
      JS.Key ("containerName");
      Optional_String'Write (S, V.containerName);
      JS.End_Object;
   end Write_SymbolInformation;

   procedure Read_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Read (S, V.partialResultToken);
      JS.Key ("query");
      LSP.Types.Read (S, V.query);
      JS.End_Object;
   end Read_WorkspaceSymbolParams;

   procedure Write_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("query");
      LSP.Types.Write (S, V.query);
      JS.End_Object;
   end Write_WorkspaceSymbolParams;

   procedure Read_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("diagnostics");
      Diagnostic_Vector'Read (S, V.diagnostics);
      JS.Key ("only");
      Optional_CodeActionKindSet'Read (S, V.only);
      JS.End_Object;
   end Read_CodeActionContext;

   procedure Write_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("diagnostics");
      Diagnostic_Vector'Write (S, V.diagnostics);
      JS.Key ("only");
      Optional_CodeActionKindSet'Write (S, V.only);
      JS.End_Object;
   end Write_CodeActionContext;

   procedure Read_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Read (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("range");
      LSP.Messages.Span'Read (S, V.span);
      JS.Key ("context");
      CodeActionContext'Read (S, V.context);
      JS.End_Object;
   end Read_CodeActionParams;

   procedure Write_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("context");
      CodeActionContext'Write (S, V.context);
      JS.End_Object;
   end Write_CodeActionParams;

   procedure Read_FormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("tabSize");
      LSP_Number'Read (S, V.tabSize);
      Read_Boolean (JS, +"insertSpaces", V.insertSpaces);
      JS.Key ("trimTrailingWhitespace");
      Optional_Boolean'Read (S, V.trimTrailingWhitespace);
      JS.Key ("insertFinalNewline");
      Optional_Boolean'Read (S, V.insertFinalNewline);
      JS.Key ("trimFinalNewlines");
      Optional_Boolean'Read (S, V.trimFinalNewlines);
      JS.End_Object;
   end Read_FormattingOptions;

   procedure Write_FormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("tabSize");
      LSP_Number'Write (S, V.tabSize);
      Write_Boolean (JS, +"insertSpaces", V.insertSpaces);
      JS.Key ("trimTrailingWhitespace");
      Optional_Boolean'Write (S, V.trimTrailingWhitespace);
      JS.Key ("insertFinalNewline");
      Optional_Boolean'Write (S, V.insertFinalNewline);
      JS.Key ("trimFinalNewlines");
      Optional_Boolean'Write (S, V.trimFinalNewlines);
      JS.End_Object;
   end Write_FormattingOptions;

   procedure Read_DocumentFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("options");
      FormattingOptions'Read (S, V.options);
      JS.End_Object;
   end Read_DocumentFormattingParams;

   procedure Write_DocumentFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("options");
      FormattingOptions'Write (S, V.options);
      JS.End_Object;
   end Write_DocumentFormattingParams;

   procedure Read_DocumentRangeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentRangeFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("range");
      LSP.Messages.Span'Read (S, V.span);
      JS.Key ("options");
      FormattingOptions'Read (S, V.options);
      JS.End_Object;
   end Read_DocumentRangeFormattingParams;

   procedure Write_DocumentRangeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentRangeFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("options");
      FormattingOptions'Write (S, V.options);
      JS.End_Object;
   end Write_DocumentRangeFormattingParams;

   procedure Read_DocumentOnTypeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Read (S, V.position);
      JS.Key ("ch");
      LSP.Types.Read (S, V.ch);
      JS.Key ("options");
      FormattingOptions'Read (S, V.options);
      JS.End_Object;
   end Read_DocumentOnTypeFormattingParams;

   procedure Write_DocumentOnTypeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.Key ("ch");
      LSP.Types.Write (S, V.ch);
      JS.Key ("options");
      FormattingOptions'Write (S, V.options);
      JS.End_Object;
   end Write_DocumentOnTypeFormattingParams;

   procedure Read_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Read (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("newName");
      LSP.Types.Read (S, V.newName);
      JS.End_Object;
   end Read_RenameParams;

   procedure Write_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("newName");
      LSP.Types.Write (S, V.newName);
      JS.End_Object;
   end Write_RenameParams;

   procedure Read_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ApplyWorkspaceEditParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      Optional_String'Read (S, V.label);
      JS.Key ("edit");
      WorkspaceEdit'Read (S, V.edit);
      JS.End_Object;
   end Read_ApplyWorkspaceEditParams;

   procedure Write_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      Optional_String'Write (S, V.label);
      JS.Key ("edit");
      WorkspaceEdit'Write (S, V.edit);
      JS.End_Object;
   end Write_ApplyWorkspaceEditParams;

   procedure Read_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ApplyWorkspaceEditResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Read_Boolean (JS, +"applied", V.applied);
      JS.Key ("failureReason");
      Optional_String'Read (S, V.failureReason);
      JS.End_Object;
   end Read_ApplyWorkspaceEditResult;

   procedure Write_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Boolean (JS, +"applied", V.applied);
      JS.Key ("failureReason");
      Optional_String'Write (S, V.failureReason);
      JS.End_Object;
   end Write_ApplyWorkspaceEditResult;

   procedure Read_WorkDoneProgressBegin
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressBegin)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      LSP.Types.Read (S, V.kind);
      JS.Key ("title");
      LSP.Types.Read (S, V.title);
      JS.Key ("cancellable");
      Optional_Boolean'Read (S, V.cancellable);
      JS.Key ("message");
      Optional_String'Read (S, V.message);
      JS.Key ("percentage");
      Optional_Number'Read (S, V.percentage);
      JS.End_Object;
   end Read_WorkDoneProgressBegin;

   procedure Write_WorkDoneProgressBegin
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressBegin)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      LSP.Types.Write (S, V.kind);
      JS.Key ("title");
      LSP.Types.Write (S, V.title);
      JS.Key ("cancellable");
      Optional_Boolean'Write (S, V.cancellable);
      JS.Key ("message");
      Optional_String'Write (S, V.message);
      JS.Key ("percentage");
      Optional_Number'Write (S, V.percentage);
      JS.End_Object;
   end Write_WorkDoneProgressBegin;

   procedure Read_WorkDoneProgressReport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressReport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      LSP.Types.Read (S, V.kind);
      JS.Key ("cancellable");
      Optional_Boolean'Read (S, V.cancellable);
      JS.Key ("message");
      Optional_String'Read (S, V.message);
      JS.Key ("percentage");
      Optional_Number'Read (S, V.percentage);
      JS.End_Object;
   end Read_WorkDoneProgressReport;

   procedure Write_WorkDoneProgressReport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressReport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      LSP.Types.Write (S, V.kind);
      JS.Key ("cancellable");
      Optional_Boolean'Write (S, V.cancellable);
      JS.Key ("message");
      Optional_String'Write (S, V.message);
      JS.Key ("percentage");
      Optional_Number'Write (S, V.percentage);
      JS.End_Object;
   end Write_WorkDoneProgressReport;

   procedure Read_WorkDoneProgressEnd
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressEnd)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      LSP.Types.Read (S, V.kind);
      JS.Key ("message");
      Optional_String'Read (S, V.message);
      JS.End_Object;
   end Read_WorkDoneProgressEnd;

   procedure Write_WorkDoneProgressEnd
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressEnd)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      LSP.Types.Write (S, V.kind);
      JS.Key ("message");
      Optional_String'Write (S, V.message);
      JS.End_Object;
   end Write_WorkDoneProgressEnd;

   procedure Read_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFoldersChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("added");
      WorkspaceFolder_Vector'Read (S, V.added);
      JS.Key ("removed");
      WorkspaceFolder_Vector'Read (S, V.removed);
      JS.End_Object;
   end Read_WorkspaceFoldersChangeEvent;

   procedure Write_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFoldersChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("added");
      WorkspaceFolder_Vector'Write (S, V.added);
      JS.Key ("removed");
      WorkspaceFolder_Vector'Write (S, V.removed);
      JS.End_Object;
   end Write_WorkspaceFoldersChangeEvent;

   procedure Read_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeWorkspaceFoldersParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("event");
      WorkspaceFoldersChangeEvent'Read (S, V.event);
      JS.End_Object;
   end Read_DidChangeWorkspaceFoldersParams;

   procedure Write_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWorkspaceFoldersParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("event");
      WorkspaceFoldersChangeEvent'Write (S, V.event);
      JS.End_Object;
   end Write_DidChangeWorkspaceFoldersParams;

   procedure Read_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("scopeUri");
      Optional_String'Read (S, V.scopeUri);
      JS.Key ("section");
      Optional_String'Read (S, V.section);
      JS.End_Object;
   end Read_ConfigurationItem;

   procedure Write_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("scopeUri");
      Optional_String'Write (S, V.scopeUri);
      JS.Key ("section");
      Optional_String'Write (S, V.section);
      JS.End_Object;
   end Write_ConfigurationItem;

   procedure Read_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("items");
      ConfigurationItem_Vector'Read (S, V.items);
      JS.End_Object;
   end Read_ConfigurationParams;

   procedure Write_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("items");
      ConfigurationItem_Vector'Write (S, V.items);
      JS.End_Object;
   end Write_ConfigurationParams;

   procedure Read_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileSystemWatcher)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("globPattern");
      LSP.Types.Read (S, V.globPattern);
      JS.Key ("kind");
      WatchKind_Set'Read (S, V.kind);
      JS.End_Object;
   end Read_FileSystemWatcher;

   procedure Write_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileSystemWatcher)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("globPattern");
      LSP.Types.Write (S, V.globPattern);
      JS.Key ("kind");
      WatchKind_Set'Write (S, V.kind);
      JS.End_Object;
   end Write_FileSystemWatcher;

   procedure Read_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeWatchedFilesRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("watchers");
      FileSystemWatcher_Vector'Read (S, V.watchers);
      JS.End_Object;
   end Read_DidChangeWatchedFilesRegistrationOptions;

   procedure Write_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWatchedFilesRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("watchers");
      FileSystemWatcher_Vector'Write (S, V.watchers);
      JS.End_Object;
   end Write_DidChangeWatchedFilesRegistrationOptions;

   procedure Read_CompletionTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionTriggerKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := CompletionTriggerKind'Val (JS.Read.Get - 1);
   end Read_CompletionTriggerKind;

   procedure Write_CompletionTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionTriggerKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write
        (GNATCOLL.JSON.Create
           (Integer'(CompletionTriggerKind'Pos (V)) + 1));
   end Write_CompletionTriggerKind;

   procedure Read_CompletionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("triggerKind");
      CompletionTriggerKind'Read (S, V.triggerKind);
      JS.Key ("triggerCharacter");
      Optional_String'Read (S, V.triggerCharacter);
      JS.End_Object;
   end Read_CompletionContext;

   procedure Write_CompletionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("triggerKind");
      CompletionTriggerKind'Write (S, V.triggerKind);
      JS.Key ("triggerCharacter");
      Optional_String'Write (S, V.triggerCharacter);
      JS.End_Object;
   end Write_CompletionContext;

   procedure Read_CompletionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Read (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Read (S, V.partialResultToken);
      JS.Key ("context");
      Optional_CompletionContext'Read (S, V.context);
      JS.End_Object;
   end Read_CompletionParams;

   procedure Write_CompletionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("context");
      Optional_CompletionContext'Write (S, V.context);
      JS.End_Object;
   end Write_CompletionParams;

   procedure Read_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RGBA_Color)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("red");
      LSP_Number'Read (S, V.red);
      JS.Key ("green");
      LSP_Number'Read (S, V.green);
      JS.Key ("blue");
      LSP_Number'Read (S, V.blue);
      JS.Key ("alpha");
      LSP_Number'Read (S, V.alpha);
      JS.End_Object;
   end Read_RGBA_Color;

   procedure Write_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RGBA_Color)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("red");
      LSP_Number'Write (S, V.red);
      JS.Key ("green");
      LSP_Number'Write (S, V.green);
      JS.Key ("blue");
      LSP_Number'Write (S, V.blue);
      JS.Key ("alpha");
      LSP_Number'Write (S, V.alpha);
      JS.End_Object;
   end Write_RGBA_Color;

   procedure Read_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ColorInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Read (S, V.span);
      JS.Key ("color");
      RGBA_Color'Read (S, V.color);
      JS.End_Object;
   end Read_ColorInformation;

   procedure Write_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("color");
      RGBA_Color'Write (S, V.color);
      JS.End_Object;
   end Write_ColorInformation;

   procedure Read_ColorPresentationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ColorPresentationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Read (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("color");
      RGBA_Color'Read (S, V.color);
      JS.Key ("range");
      LSP.Messages.Span'Read (S, V.span);
      JS.End_Object;
   end Read_ColorPresentationParams;

   procedure Write_ColorPresentationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorPresentationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("color");
      RGBA_Color'Write (S, V.color);
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.End_Object;
   end Write_ColorPresentationParams;

   procedure Read_ColorPresentation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ColorPresentation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      LSP.Types.Read (S, V.label);
      JS.Key ("textEdit");
      Optional_TextEdit'Read (S, V.textEdit);
      JS.Key ("additionalTextEdits");
      TextEdit_Vector'Read (S, V.additionalTextEdits);
      JS.End_Object;
   end Read_ColorPresentation;

   procedure Write_ColorPresentation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorPresentation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      LSP.Types.Write (S, V.label);
      JS.Key ("textEdit");
      Optional_TextEdit'Write (S, V.textEdit);
      JS.Key ("additionalTextEdits");
      TextEdit_Vector'Write (S, V.additionalTextEdits);
      JS.End_Object;
   end Write_ColorPresentation;

   procedure Read_FoldingRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FoldingRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Read (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.End_Object;
   end Read_FoldingRangeParams;

   procedure Write_FoldingRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_FoldingRangeParams;

   procedure Read_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FoldingRange)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("startLine");
      Line_Number'Read (S, V.startLine);
      JS.Key ("startCharacter");
      Optional_Number'Read (S, V.startCharacter);
      JS.Key ("endLine");
      Line_Number'Read (S, V.endLine);
      JS.Key ("endCharacter");
      Optional_Number'Read (S, V.endCharacter);
      JS.Key ("kind");
      Optional_String'Read (S, V.kind);
      JS.End_Object;
   end Read_FoldingRange;

   procedure Write_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRange)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("startLine");
      Line_Number'Write (S, V.startLine);
      JS.Key ("startCharacter");
      Optional_Number'Write (S, V.startCharacter);
      JS.Key ("endLine");
      Line_Number'Write (S, V.endLine);
      JS.Key ("endCharacter");
      Optional_Number'Write (S, V.endCharacter);
      JS.Key ("kind");
      Optional_String'Write (S, V.kind);
      JS.End_Object;
   end Write_FoldingRange;

   procedure Read_DocumentColorParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentColorParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Read (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.End_Object;
   end Read_DocumentColorParams;

   procedure Write_DocumentColorParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentColorParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DocumentColorParams;

   procedure Read_SelectionRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SelectionRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Read (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Read (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("positions");
      Position_Vector'Read (S, V.positions);
      JS.End_Object;
   end Read_SelectionRangeParams;

   procedure Write_SelectionRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SelectionRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("positions");
      Position_Vector'Write (S, V.positions);
      JS.End_Object;
   end Write_SelectionRangeParams;

   procedure Read_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SelectionRange)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Read (S, V.span);
      JS.End_Object;
   end Read_SelectionRange;

   procedure Write_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SelectionRange)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.End_Object;
   end Write_SelectionRange;

   procedure Read_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Subprogram_And_References)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("location");
      Location'Read (S, V.loc);
      JS.Key ("name");
      LSP.Types.Read (S, V.name);
      JS.Key ("refs");
      Location_Vector'Read (S, V.refs);
      JS.End_Object;
   end Read_ALS_Subprogram_And_References;

   procedure Write_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Subprogram_And_References)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("location");
      Location'Write (S, V.loc);
      JS.Key ("name");
      LSP.Types.Write (S, V.name);
      JS.Key ("refs");
      Location_Vector'Write (S, V.refs);
      JS.End_Object;
   end Write_ALS_Subprogram_And_References;

end LSP.Message_IO;
