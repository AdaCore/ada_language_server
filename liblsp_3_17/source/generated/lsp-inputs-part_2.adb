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

package body LSP.Inputs.Part_2 is

   package WorkspaceFoldersInitializeParams_Scope is
      package WorkspaceFoldersInitializeParams_Map is new Minimal_Perfect_Hash
        (["workspaceFolders"]);

   end WorkspaceFoldersInitializeParams_Scope;

   procedure Read_WorkspaceFoldersInitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFoldersInitializeParams) is
      use WorkspaceFoldersInitializeParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceFoldersInitializeParams_Map.Get_Index (Key) is
               when 1 =>  --  workspaceFolders
                  Value.workspaceFolders :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WorkspaceFolder_Vector_Or_Null
                    (Handler, Value.workspaceFolders.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceFoldersInitializeParams;

   package symbolKind_OfWorkspaceSymbolClientCapabilities_Scope is
      package symbolKind_OfWorkspaceSymbolClientCapabilities_Map is new Minimal_Perfect_Hash
        (["valueSet"]);

   end symbolKind_OfWorkspaceSymbolClientCapabilities_Scope;

   procedure Read_symbolKind_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .symbolKind_OfWorkspaceSymbolClientCapabilities) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            use symbolKind_OfWorkspaceSymbolClientCapabilities_Scope;
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case symbolKind_OfWorkspaceSymbolClientCapabilities_Map.Get_Index
              (Key) is
               when 1 =>  --  valueSet
                  Read_SymbolKind_Set (Handler, Value.valueSet);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_symbolKind_OfWorkspaceSymbolClientCapabilities;

   procedure Read_T
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.T) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            Handler.Skip_Current_Value;
         end;
      end loop;

      Handler.Read_Next;
   end Read_T;

   package ApplyWorkspaceEditResult_Scope is
      package ApplyWorkspaceEditResult_Map is new Minimal_Perfect_Hash
        (["applied",
         "failureReason",
         "failedChange"]);

   end ApplyWorkspaceEditResult_Scope;

   procedure Read_ApplyWorkspaceEditResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ApplyWorkspaceEditResult) is
      use ApplyWorkspaceEditResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ApplyWorkspaceEditResult_Map.Get_Index (Key) is
               when 1 =>  --  applied
                  Value.applied := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  failureReason
                  Value.failureReason.Clear;
                  Value.failureReason.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  failedChange
                  Value.failedChange       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.failedChange.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ApplyWorkspaceEditResult;

   package SignatureHelpRegistrationOptions_Scope is
      package SignatureHelpRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "triggerCharacters",
         "retriggerCharacters"]);

   end SignatureHelpRegistrationOptions_Scope;

   procedure Read_SignatureHelpRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpRegistrationOptions) is
      use SignatureHelpRegistrationOptions_Scope;
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
            case SignatureHelpRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  triggerCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.triggerCharacters);
               when 4 =>  --  retriggerCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.retriggerCharacters);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureHelpRegistrationOptions;

   package InlayHintParams_Scope is
      package InlayHintParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "textDocument",
         "range"]);

   end InlayHintParams_Scope;

   procedure Read_InlayHintParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintParams) is
      use InlayHintParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlayHintParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 3 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlayHintParams;

   package WorkDoneProgressCreateParams_Scope is
      package WorkDoneProgressCreateParams_Map is new Minimal_Perfect_Hash
        (["token"]);

   end WorkDoneProgressCreateParams_Scope;

   procedure Read_WorkDoneProgressCreateParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressCreateParams) is
      use WorkDoneProgressCreateParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkDoneProgressCreateParams_Map.Get_Index (Key) is
               when 1 =>  --  token
                  Read_ProgressToken (Handler, Value.token);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkDoneProgressCreateParams;

   package RegularExpressionsClientCapabilities_Scope is
      package RegularExpressionsClientCapabilities_Map is new Minimal_Perfect_Hash
        (["engine",
         "version"]);

   end RegularExpressionsClientCapabilities_Scope;

   procedure Read_RegularExpressionsClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RegularExpressionsClientCapabilities) is
      use RegularExpressionsClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RegularExpressionsClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  engine
                  Value.engine.Clear;
                  Value.engine.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  version
                  Value.version.Clear;
                  Value.version.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RegularExpressionsClientCapabilities;

   package DiagnosticRelatedInformation_Scope is
      package DiagnosticRelatedInformation_Map is new Minimal_Perfect_Hash
        (["location",
         "message"]);

   end DiagnosticRelatedInformation_Scope;

   procedure Read_DiagnosticRelatedInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticRelatedInformation) is
      use DiagnosticRelatedInformation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DiagnosticRelatedInformation_Map.Get_Index (Key) is
               when 1 =>  --  location
                  Read_Location (Handler, Value.location);
               when 2 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DiagnosticRelatedInformation;

   package DocumentFormattingParams_Scope is
      package DocumentFormattingParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "textDocument",
         "options"]);

   end DocumentFormattingParams_Scope;

   procedure Read_DocumentFormattingParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFormattingParams) is
      use DocumentFormattingParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentFormattingParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 3 =>  --  options
                  Read_FormattingOptions (Handler, Value.options);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentFormattingParams;

   procedure Read_CompletionTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionTriggerKind) is
   begin
      Value :=
        LSP.Enumerations.CompletionTriggerKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_CompletionTriggerKind;

   procedure Read_InlineValue_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValue_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.InlineValue_Vector renames Value;
         Value : LSP.Structures.InlineValue;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_InlineValue (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_InlineValue_Vector;

   package InlineValueEvaluatableExpression_Scope is
      package InlineValueEvaluatableExpression_Map is new Minimal_Perfect_Hash
        (["range",
         "expression"]);

   end InlineValueEvaluatableExpression_Scope;

   procedure Read_InlineValueEvaluatableExpression
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueEvaluatableExpression) is
      use InlineValueEvaluatableExpression_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueEvaluatableExpression_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  expression
                  Value.expression.Clear;
                  Value.expression.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueEvaluatableExpression;

   procedure Read_SymbolTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SymbolTag) is
   begin
      Value :=
        LSP.Enumerations.SymbolTag'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_SymbolTag;

   package CompletionOptions_Scope is
      package CompletionOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "triggerCharacters",
         "allCommitCharacters",
         "resolveProvider",
         "completionItem"]);

      package completionItem_OfCompletionOptions_Scope is
         package completionItem_OfCompletionOptions_Map is new Minimal_Perfect_Hash
           (["labelDetailsSupport"]);

      end completionItem_OfCompletionOptions_Scope;

   end CompletionOptions_Scope;

   procedure Read_CompletionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionOptions) is
      use CompletionOptions_Scope;
      procedure Read_completionItem_OfCompletionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.completionItem_OfCompletionOptions);

      procedure Read_completionItem_OfCompletionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.completionItem_OfCompletionOptions) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use completionItem_OfCompletionOptions_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case completionItem_OfCompletionOptions_Map.Get_Index (Key) is
                  when 1 =>  --  labelDetailsSupport
                     Value.labelDetailsSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.labelDetailsSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_completionItem_OfCompletionOptions;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  triggerCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.triggerCharacters);
               when 3 =>  --  allCommitCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.allCommitCharacters);
               when 4 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  completionItem
                  Value.completionItem :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_completionItem_OfCompletionOptions
                    (Handler, Value.completionItem.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionOptions;

end LSP.Inputs.Part_2;
