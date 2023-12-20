--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

pragma Ada_2022;
pragma Warnings (Off, "is not referenced");
with Interfaces;
with LSP.Input_Tools;
with VSS.Strings;
with VSS.JSON.Pull_Readers.Buffered;
with Minimal_Perfect_Hash;

package body LSP.Inputs.Part_21 is

   package InitializeResult_Scope is
      package InitializeResult_Map is new Minimal_Perfect_Hash
        (["capabilities",
         "serverInfo"]);

   end InitializeResult_Scope;

   procedure Read_InitializeResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializeResult) is
      use InitializeResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InitializeResult_Map.Get_Index (Key) is
               when 1 =>  --  capabilities
                  Read_ServerCapabilities (Handler, Value.capabilities);
               when 2 =>  --  serverInfo
                  Value.serverInfo :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_clientInfo_Of_InitializeParams
                    (Handler, Value.serverInfo.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InitializeResult;

   package DidChangeTextDocumentParams_Scope is
      package DidChangeTextDocumentParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "contentChanges"]);

   end DidChangeTextDocumentParams_Scope;

   procedure Read_DidChangeTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeTextDocumentParams) is
      use DidChangeTextDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeTextDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_VersionedTextDocumentIdentifier
                    (Handler, Value.textDocument);
               when 2 =>  --  contentChanges
                  Read_TextDocumentContentChangeEvent_Vector
                    (Handler, Value.contentChanges);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeTextDocumentParams;

   procedure Read_CallHierarchyOutgoingCall_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOutgoingCall_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_CallHierarchyOutgoingCall_Vector (Handler, Value);
      end if;
   end Read_CallHierarchyOutgoingCall_Vector_Or_Null;

   procedure Read_WatchKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.WatchKind) is
   begin
      Value := LSP.Enumerations.WatchKind (Handler.Number_Value.Integer_Value);
      Handler.Read_Next;
   end Read_WatchKind;

   package DocumentOnTypeFormattingOptions_Scope is
      package DocumentOnTypeFormattingOptions_Map is new Minimal_Perfect_Hash
        (["firstTriggerCharacter",
         "moreTriggerCharacter"]);

   end DocumentOnTypeFormattingOptions_Scope;

   procedure Read_DocumentOnTypeFormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentOnTypeFormattingOptions) is
      use DocumentOnTypeFormattingOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentOnTypeFormattingOptions_Map.Get_Index (Key) is
               when 1 =>  --  firstTriggerCharacter
                  Value.firstTriggerCharacter.Clear;
                  Value.firstTriggerCharacter.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  moreTriggerCharacter
                  Read_Virtual_String_Vector
                    (Handler, Value.moreTriggerCharacter);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentOnTypeFormattingOptions;

   package RenameRegistrationOptions_Scope is
      package RenameRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "prepareProvider"]);

   end RenameRegistrationOptions_Scope;

   procedure Read_RenameRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameRegistrationOptions) is
      use RenameRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  prepareProvider
                  Value.prepareProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.prepareProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameRegistrationOptions;

   package MonikerKind_Map is new Minimal_Perfect_Hash
     (["import",
      "export",
      "local"]);

   procedure Read_MonikerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.MonikerKind) is
   begin
      Value :=
        LSP.Enumerations.MonikerKind'Val
          (MonikerKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_MonikerKind;

   package CallHierarchyIncomingCall_Scope is
      package CallHierarchyIncomingCall_Map is new Minimal_Perfect_Hash
        (["from",
         "fromRanges",
         "dispatching_calls"]);

   end CallHierarchyIncomingCall_Scope;

   procedure Read_CallHierarchyIncomingCall
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyIncomingCall) is
      use CallHierarchyIncomingCall_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyIncomingCall_Map.Get_Index (Key) is
               when 1 =>  --  from
                  Read_CallHierarchyItem (Handler, Value.from);
               when 2 =>  --  fromRanges
                  Read_Range_Vector (Handler, Value.fromRanges);
               when 3 =>  --  dispatching_calls
                  Read_Boolean_Vector (Handler, Value.dispatching_calls);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyIncomingCall;

   package resolveSupport_OfWorkspaceSymbolClientCapabilities_Scope is
      package resolveSupport_OfWorkspaceSymbolClientCapabilities_Map is new Minimal_Perfect_Hash
        (["properties"]);

   end resolveSupport_OfWorkspaceSymbolClientCapabilities_Scope;

   procedure Read_resolveSupport_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .resolveSupport_OfWorkspaceSymbolClientCapabilities) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            use resolveSupport_OfWorkspaceSymbolClientCapabilities_Scope;
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case resolveSupport_OfWorkspaceSymbolClientCapabilities_Map
              .Get_Index
              (Key) is
               when 1 =>  --  properties
                  Read_Virtual_String_Vector (Handler, Value.properties);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_resolveSupport_OfWorkspaceSymbolClientCapabilities;

   procedure Read_TextDocumentItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentItem_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.TextDocumentItem_Vector renames Value;
         Value : LSP.Structures.TextDocumentItem;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_TextDocumentItem (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_TextDocumentItem_Vector;

   package TextEdit_Scope is
      package TextEdit_Map is new Minimal_Perfect_Hash
        (["range",
         "newText"]);

   end TextEdit_Scope;

   procedure Read_TextEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextEdit) is
      use TextEdit_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextEdit_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  newText
                  Value.newText.Clear;
                  Value.newText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextEdit;

   package SemanticTokensRangeParams_Scope is
      package SemanticTokensRangeParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "range"]);

   end SemanticTokensRangeParams_Scope;

   procedure Read_SemanticTokensRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensRangeParams) is
      use SemanticTokensRangeParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensRangeParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 4 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensRangeParams;

   package TextDocumentContentChangeEvent_Scope is
      package TextDocumentContentChangeEvent_Map is new Minimal_Perfect_Hash
        (["range",
         "rangeLength",
         "text"]);

   end TextDocumentContentChangeEvent_Scope;

   procedure Read_TextDocumentContentChangeEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentContentChangeEvent) is
      use TextDocumentContentChangeEvent_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentContentChangeEvent_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Value.a_range :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_A_Range (Handler, Value.a_range.Value);
               when 2 =>  --  rangeLength
                  Value.rangeLength       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.rangeLength.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  text
                  Value.text.Clear;
                  Value.text.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentContentChangeEvent;

   package A_Range_Scope is
      package A_Range_Map is new Minimal_Perfect_Hash
        (["start",
         "end"]);

   end A_Range_Scope;

   procedure Read_A_Range
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.A_Range) is
      use A_Range_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case A_Range_Map.Get_Index (Key) is
               when 1 =>  --  start
                  Read_Position (Handler, Value.start);
               when 2 =>  --  end
                  Read_Position (Handler, Value.an_end);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_A_Range;

   package TextDocumentIdentifier_Scope is
      package TextDocumentIdentifier_Map is new Minimal_Perfect_Hash (["uri"]);

   end TextDocumentIdentifier_Scope;

   procedure Read_TextDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentIdentifier) is
      use TextDocumentIdentifier_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentIdentifier_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentIdentifier;

   package WillSaveTextDocumentParams_Scope is
      package WillSaveTextDocumentParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "reason"]);

   end WillSaveTextDocumentParams_Scope;

   procedure Read_WillSaveTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WillSaveTextDocumentParams) is
      use WillSaveTextDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WillSaveTextDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  reason
                  Read_TextDocumentSaveReason (Handler, Value.reason);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WillSaveTextDocumentParams;

   package Registration_Scope is
      package Registration_Map is new Minimal_Perfect_Hash
        (["id",
         "method",
         "registerOptions"]);

   end Registration_Scope;

   procedure Read_Registration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Registration) is
      use Registration_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Registration_Map.Get_Index (Key) is
               when 1 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  method
                  Value.method.Clear;
                  Value.method.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  registerOptions
                  Read_LSPAny (Handler, Value.registerOptions);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Registration;

   package CompletionItem_Scope is
      package CompletionItem_Map is new Minimal_Perfect_Hash
        (["label",
         "labelDetails",
         "kind",
         "tags",
         "detail",
         "documentation",
         "deprecated",
         "preselect",
         "sortText",
         "filterText",
         "insertText",
         "insertTextFormat",
         "insertTextMode",
         "textEdit",
         "textEditText",
         "additionalTextEdits",
         "commitCharacters",
         "command",
         "data"]);

      package TextEdit_Or_InsertReplaceEdit_Scope is
         package TextEdit_Or_InsertReplaceEdit_Map is new Minimal_Perfect_Hash
           (["range",
            "insert",
            "replace"]);

      end TextEdit_Or_InsertReplaceEdit_Scope;

   end CompletionItem_Scope;

   procedure Read_CompletionItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItem) is
      use CompletionItem_Scope;
      procedure Read_TextEdit_Or_InsertReplaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TextEdit_Or_InsertReplaceEdit);

      procedure Read_TextEdit_Or_InsertReplaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TextEdit_Or_InsertReplaceEdit) is
         use TextEdit_Or_InsertReplaceEdit_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       TextEdit_Or_InsertReplaceEdit_Map.Get_Index (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  range
                           Value :=
                             (Is_TextEdit => True,
                              others      => <>);
                           exit;
                        when 2 =>  --  insert
                           Value :=
                             (Is_TextEdit => False,
                              others      => <>);
                           exit;
                        when 3 =>  --  replace
                           Value :=
                             (Is_TextEdit => False,
                              others      => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Is_TextEdit is
               when True =>
                  Read_TextEdit (Handler, Value.TextEdit);
               when False =>
                  Read_InsertReplaceEdit (Handler, Value.InsertReplaceEdit);
            end case;
         end;
      end Read_TextEdit_Or_InsertReplaceEdit;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionItem_Map.Get_Index (Key) is
               when 1 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  labelDetails
                  Value.labelDetails :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CompletionItemLabelDetails
                    (Handler, Value.labelDetails.Value);
               when 3 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CompletionItemKind (Handler, Value.kind.Value);
               when 4 =>  --  tags
                  Read_CompletionItemTag_Set (Handler, Value.tags);
               when 5 =>  --  detail
                  Value.detail.Clear;
                  Value.detail.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 6 =>  --  documentation
                  Value.documentation :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_MarkupContent
                    (Handler, Value.documentation.Value);
               when 7 =>  --  deprecated
                  Value.deprecated       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.deprecated.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 8 =>  --  preselect
                  Value.preselect       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.preselect.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 9 =>  --  sortText
                  Value.sortText.Clear;
                  Value.sortText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 10 =>  --  filterText
                  Value.filterText.Clear;
                  Value.filterText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 11 =>  --  insertText
                  Value.insertText.Clear;
                  Value.insertText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 12 =>  --  insertTextFormat
                  Value.insertTextFormat :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InsertTextFormat
                    (Handler, Value.insertTextFormat.Value);
               when 13 =>  --  insertTextMode
                  Value.insertTextMode :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InsertTextMode (Handler, Value.insertTextMode.Value);
               when 14 =>  --  textEdit
                  Value.textEdit :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TextEdit_Or_InsertReplaceEdit
                    (Handler, Value.textEdit.Value);
               when 15 =>  --  textEditText
                  Value.textEditText.Clear;
                  Value.textEditText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 16 =>  --  additionalTextEdits
                  Read_TextEdit_Vector (Handler, Value.additionalTextEdits);
               when 17 =>  --  commitCharacters
                  Read_Virtual_String_Vector (Handler, Value.commitCharacters);
               when 18 =>  --  command
                  Value.command :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Command (Handler, Value.command.Value);
               when 19 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionItem;

end LSP.Inputs.Part_21;
