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

package body LSP.Inputs.Part_1 is

   package NotebookDocumentIdentifier_Scope is
      package NotebookDocumentIdentifier_Map is new Minimal_Perfect_Hash
        (["uri"]);

   end NotebookDocumentIdentifier_Scope;

   procedure Read_NotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentIdentifier) is
      use NotebookDocumentIdentifier_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentIdentifier_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Read_URI (Handler, Value.uri);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocumentIdentifier;

   package SignatureHelp_Scope is
      package SignatureHelp_Map is new Minimal_Perfect_Hash
        (["signatures",
         "activeSignature",
         "activeParameter"]);

   end SignatureHelp_Scope;

   procedure Read_SignatureHelp
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelp) is
      use SignatureHelp_Scope;
      procedure Read_SignatureInformation_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.SignatureInformation_Vector);

      procedure Read_SignatureInformation_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.SignatureInformation_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.SignatureInformation_Vector renames Value;
            Value : LSP.Structures.SignatureInformation;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_SignatureInformation (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_SignatureInformation_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureHelp_Map.Get_Index (Key) is
               when 1 =>  --  signatures
                  Read_SignatureInformation_Vector (Handler, Value.signatures);
               when 2 =>  --  activeSignature
                  Value.activeSignature       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.activeSignature.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  activeParameter
                  Value.activeParameter       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.activeParameter.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureHelp;

   package ShowDocumentResult_Scope is
      package ShowDocumentResult_Map is new Minimal_Perfect_Hash (["success"]);

   end ShowDocumentResult_Scope;

   procedure Read_ShowDocumentResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowDocumentResult) is
      use ShowDocumentResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ShowDocumentResult_Map.Get_Index (Key) is
               when 1 =>  --  success
                  Value.success := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ShowDocumentResult;

   package CompletionRegistrationOptions_Scope is
      package CompletionRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "triggerCharacters",
         "allCommitCharacters",
         "resolveProvider",
         "completionItem"]);

      package completionItem_OfCompletionOptions_Scope is
         package completionItem_OfCompletionOptions_Map is new Minimal_Perfect_Hash
           (["labelDetailsSupport"]);

      end completionItem_OfCompletionOptions_Scope;

   end CompletionRegistrationOptions_Scope;

   procedure Read_CompletionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionRegistrationOptions) is
      use CompletionRegistrationOptions_Scope;
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
            case CompletionRegistrationOptions_Map.Get_Index (Key) is
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
               when 4 =>  --  allCommitCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.allCommitCharacters);
               when 5 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  completionItem
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
   end Read_CompletionRegistrationOptions;

   procedure Read_SignatureHelpTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SignatureHelpTriggerKind) is
   begin
      Value :=
        LSP.Enumerations.SignatureHelpTriggerKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_SignatureHelpTriggerKind;

   package MonikerParams_Scope is
      package MonikerParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken"]);

   end MonikerParams_Scope;

   procedure Read_MonikerParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerParams) is
      use MonikerParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MonikerParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 4 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_MonikerParams;

   procedure Read_DiagnosticTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DiagnosticTag) is
   begin
      Value :=
        LSP.Enumerations.DiagnosticTag'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_DiagnosticTag;

   procedure Read_WorkspaceSymbol_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbol_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.WorkspaceSymbol_Vector renames Value;
         Value : LSP.Structures.WorkspaceSymbol;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_WorkspaceSymbol (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_WorkspaceSymbol_Vector;

   package SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult_Scope
   is
      package SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult_Map is new Minimal_Perfect_Hash
        (["data",
         "edits"]);

   end SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult_Scope;

   procedure Read_SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult) is
      use
        SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult_Map
                      .Get_Index
                      (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  data
                        Value :=
                          (Is_SemanticTokensPartialResult => True,
                           others                         => <>);
                        exit;
                     when 2 =>  --  edits
                        Value :=
                          (Is_SemanticTokensPartialResult => False,
                           others                         => <>);
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

         case Value.Is_SemanticTokensPartialResult is
            when True =>
               Read_SemanticTokensPartialResult
                 (Handler, Value.SemanticTokensPartialResult);
            when False =>
               Read_SemanticTokensDeltaPartialResult
                 (Handler, Value.SemanticTokensDeltaPartialResult);
         end case;
      end;
   end Read_SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult;

   package SignatureHelpOptions_Scope is
      package SignatureHelpOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "triggerCharacters",
         "retriggerCharacters"]);

   end SignatureHelpOptions_Scope;

   procedure Read_SignatureHelpOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpOptions) is
      use SignatureHelpOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureHelpOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  triggerCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.triggerCharacters);
               when 3 =>  --  retriggerCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.retriggerCharacters);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureHelpOptions;

   package DidChangeNotebookDocumentParams_Scope is
      package DidChangeNotebookDocumentParams_Map is new Minimal_Perfect_Hash
        (["notebookDocument",
         "change"]);

   end DidChangeNotebookDocumentParams_Scope;

   procedure Read_DidChangeNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeNotebookDocumentParams) is
      use DidChangeNotebookDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeNotebookDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  notebookDocument
                  Read_VersionedNotebookDocumentIdentifier
                    (Handler, Value.notebookDocument);
               when 2 =>  --  change
                  Read_NotebookDocumentChangeEvent (Handler, Value.change);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeNotebookDocumentParams;

   package DocumentSymbolParams_Scope is
      package DocumentSymbolParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "query",
         "case_sensitive",
         "whole_word",
         "negate",
         "kind"]);

   end DocumentSymbolParams_Scope;

   procedure Read_DocumentSymbolParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolParams) is
      use DocumentSymbolParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentSymbolParams_Map.Get_Index (Key) is
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
               when 4 =>  --  query
                  Value.query.Clear;
                  Value.query.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  case_sensitive
                  Value.case_sensitive       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.case_sensitive.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  whole_word
                  Value.whole_word       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.whole_word.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 7 =>  --  negate
                  Value.negate       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.negate.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 8 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_AlsSearchKind (Handler, Value.kind.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentSymbolParams;

   procedure Read_WorkspaceDocumentDiagnosticReport_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDocumentDiagnosticReport_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   :
           LSP.Structures.WorkspaceDocumentDiagnosticReport_Vector renames
           Value;
         Value : LSP.Structures.WorkspaceDocumentDiagnosticReport;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_WorkspaceDocumentDiagnosticReport (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_WorkspaceDocumentDiagnosticReport_Vector;

end LSP.Inputs.Part_1;
