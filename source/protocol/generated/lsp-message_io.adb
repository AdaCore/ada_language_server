--  Automatically generated, do not edit.
with GNATCOLL.JSON;

with LSP.JSON_Streams;
with LSP.Messages;                 use LSP.Messages;
with LSP.Types;                    use LSP.Types;

package body LSP.Message_IO is
   pragma Style_Checks ("M175");

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
