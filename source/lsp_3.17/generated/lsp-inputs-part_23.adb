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

package body LSP.Inputs.Part_23 is

   package WindowClientCapabilities_Scope is
      package WindowClientCapabilities_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "showMessage",
         "showDocument"]);

   end WindowClientCapabilities_Scope;

   procedure Read_WindowClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WindowClientCapabilities) is
      use WindowClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WindowClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  showMessage
                  Value.showMessage :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ShowMessageRequestClientCapabilities
                    (Handler, Value.showMessage.Value);
               when 3 =>  --  showDocument
                  Value.showDocument :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ShowDocumentClientCapabilities
                    (Handler, Value.showDocument.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WindowClientCapabilities;

   procedure Read_SignatureHelp_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelp_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_SignatureHelp (Handler, Value.Value);
      end if;
   end Read_SignatureHelp_Or_Null;

   package DocumentLinkParams_Scope is
      package DocumentLinkParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument"]);

   end DocumentLinkParams_Scope;

   procedure Read_DocumentLinkParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkParams) is
      use DocumentLinkParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentLinkParams_Map.Get_Index (Key) is
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
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentLinkParams;

   package Declaration_Progress_Report_Scope is
      package Declaration_Progress_Report_Map is new Minimal_Perfect_Hash
        (["uri",
         "range",
         "alsKind",
         "originSelectionRange",
         "targetUri",
         "targetRange",
         "targetSelectionRange"]);

   end Declaration_Progress_Report_Scope;

   procedure Read_Declaration_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Declaration_Progress_Report) is
      use Declaration_Progress_Report_Scope;
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
         if Handler.Is_Start_Array then
            Handler.Read_Next;
         end if;
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Declaration_Progress_Report_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  uri
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  range
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 3 =>  --  alsKind
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 4 =>  --  originSelectionRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 5 =>  --  targetUri
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 6 =>  --  targetRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 7 =>  --  targetSelectionRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
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

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_Location_Vector (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_DeclarationLink_Vector (Handler, Value.Variant_2);
         end case;
      end;
   end Read_Declaration_Progress_Report;

   procedure Read_CodeActionTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CodeActionTriggerKind) is
   begin
      Value :=
        LSP.Enumerations.CodeActionTriggerKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_CodeActionTriggerKind;

   procedure Read_DefinitionLink_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionLink_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DefinitionLink_Vector renames Value;
         Value : LSP.Structures.DefinitionLink;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_DefinitionLink (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_DefinitionLink_Vector;

   package ShowDocumentClientCapabilities_Scope is
      package ShowDocumentClientCapabilities_Map is new Minimal_Perfect_Hash
        (["support"]);

   end ShowDocumentClientCapabilities_Scope;

   procedure Read_ShowDocumentClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowDocumentClientCapabilities) is
      use ShowDocumentClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ShowDocumentClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  support
                  Value.support := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ShowDocumentClientCapabilities;

   package CodeAction_Scope is
      package CodeAction_Map is new Minimal_Perfect_Hash
        (["title",
         "kind",
         "diagnostics",
         "isPreferred",
         "disabled",
         "edit",
         "command",
         "data"]);

      package disabled_OfCodeAction_Scope is
         package disabled_OfCodeAction_Map is new Minimal_Perfect_Hash
           (["reason"]);

      end disabled_OfCodeAction_Scope;

   end CodeAction_Scope;

   procedure Read_CodeAction
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeAction) is
      use CodeAction_Scope;
      procedure Read_disabled_OfCodeAction
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.disabled_OfCodeAction);

      procedure Read_disabled_OfCodeAction
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.disabled_OfCodeAction) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use disabled_OfCodeAction_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case disabled_OfCodeAction_Map.Get_Index (Key) is
                  when 1 =>  --  reason
                     Value.reason.Clear;
                     Value.reason.Append (Handler.String_Value);
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_disabled_OfCodeAction;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeAction_Map.Get_Index (Key) is
               when 1 =>  --  title
                  Value.title.Clear;
                  Value.title.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeActionKind (Handler, Value.kind.Value);
               when 3 =>  --  diagnostics
                  Read_Diagnostic_Vector (Handler, Value.diagnostics);
               when 4 =>  --  isPreferred
                  Value.isPreferred       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.isPreferred.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  disabled
                  Value.disabled :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_disabled_OfCodeAction (Handler, Value.disabled.Value);
               when 6 =>  --  edit
                  Value.edit :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WorkspaceEdit (Handler, Value.edit.Value);
               when 7 =>  --  command
                  Value.command :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Command (Handler, Value.command.Value);
               when 8 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeAction;

   package SelectionRangeParams_Scope is
      package SelectionRangeParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "positions"]);

   end SelectionRangeParams_Scope;

   procedure Read_SelectionRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeParams) is
      use SelectionRangeParams_Scope;
      procedure Read_Position_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Position_Vector);

      procedure Read_Position_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Position_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.Position_Vector renames Value;
            Value : LSP.Structures.Position;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_Position (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_Position_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SelectionRangeParams_Map.Get_Index (Key) is
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
               when 4 =>  --  positions
                  Read_Position_Vector (Handler, Value.positions);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SelectionRangeParams;

   package FileOperationFilter_Scope is
      package FileOperationFilter_Map is new Minimal_Perfect_Hash
        (["scheme",
         "pattern"]);

   end FileOperationFilter_Scope;

   procedure Read_FileOperationFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationFilter) is
      use FileOperationFilter_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileOperationFilter_Map.Get_Index (Key) is
               when 1 =>  --  scheme
                  Value.scheme.Clear;
                  Value.scheme.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  pattern
                  Read_FileOperationPattern (Handler, Value.pattern);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileOperationFilter;

   procedure Read_CompletionItemTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionItemTag) is
   begin
      Value :=
        LSP.Enumerations.CompletionItemTag'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_CompletionItemTag;

   package DidChangeWorkspaceFoldersParams_Scope is
      package DidChangeWorkspaceFoldersParams_Map is new Minimal_Perfect_Hash
        (["event"]);

   end DidChangeWorkspaceFoldersParams_Scope;

   procedure Read_DidChangeWorkspaceFoldersParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeWorkspaceFoldersParams) is
      use DidChangeWorkspaceFoldersParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeWorkspaceFoldersParams_Map.Get_Index (Key) is
               when 1 =>  --  event
                  Read_WorkspaceFoldersChangeEvent (Handler, Value.event);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeWorkspaceFoldersParams;

   package DidOpenTextDocumentParams_Scope is
      package DidOpenTextDocumentParams_Map is new Minimal_Perfect_Hash
        (["textDocument"]);

   end DidOpenTextDocumentParams_Scope;

   procedure Read_DidOpenTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidOpenTextDocumentParams) is
      use DidOpenTextDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidOpenTextDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentItem (Handler, Value.textDocument);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidOpenTextDocumentParams;

   package CodeActionOptions_Scope is
      package CodeActionOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "codeActionKinds",
         "resolveProvider"]);

   end CodeActionOptions_Scope;

   procedure Read_CodeActionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionOptions) is
      use CodeActionOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeActionOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  codeActionKinds
                  Read_CodeActionKind_Set (Handler, Value.codeActionKinds);
               when 3 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeActionOptions;

   package WorkspaceFolder_Scope is
      package WorkspaceFolder_Map is new Minimal_Perfect_Hash
        (["uri",
         "name"]);

   end WorkspaceFolder_Scope;

   procedure Read_WorkspaceFolder
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFolder) is
      use WorkspaceFolder_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceFolder_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Read_URI (Handler, Value.uri);
               when 2 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceFolder;

   package WorkspaceFoldersServerCapabilities_Scope is
      package WorkspaceFoldersServerCapabilities_Map is new Minimal_Perfect_Hash
        (["supported",
         "changeNotifications"]);

   end WorkspaceFoldersServerCapabilities_Scope;

   procedure Read_WorkspaceFoldersServerCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFoldersServerCapabilities) is
      use WorkspaceFoldersServerCapabilities_Scope;
      procedure Read_Virtual_String_Or_Boolean
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_Boolean);

      procedure Read_Virtual_String_Or_Boolean
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_Boolean) is
      begin
         if Handler.Is_String_Value then
            Value :=
              (Is_Virtual_String => True,
               others            => <>);
         else
            Value :=
              (Is_Virtual_String => False,
               others            => <>);
         end if;

         case Value.Is_Virtual_String is
            when True =>
               Value.Virtual_String.Clear;
               Value.Virtual_String.Append (Handler.String_Value);
               Handler.Read_Next;
            when False =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
         end case;
      end Read_Virtual_String_Or_Boolean;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceFoldersServerCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  supported
                  Value.supported       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.supported.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  changeNotifications
                  Value.changeNotifications :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_Boolean
                    (Handler, Value.changeNotifications.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceFoldersServerCapabilities;

   package SemanticTokensRegistrationOptions_Scope is
      package SemanticTokensRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "legend",
         "range",
         "full",
         "id"]);

   end SemanticTokensRegistrationOptions_Scope;

   procedure Read_SemanticTokensRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensRegistrationOptions) is
      use SemanticTokensRegistrationOptions_Scope;
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
            case SemanticTokensRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  legend
                  Read_SemanticTokensLegend (Handler, Value.legend);
               when 4 =>  --  range
                  Value.a_range :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_Any (Handler, Value.a_range.Value);
               when 5 =>  --  full
                  Value.full :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_Something (Handler, Value.full.Value);
               when 6 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensRegistrationOptions;

   package LogMessageParams_Scope is
      package LogMessageParams_Map is new Minimal_Perfect_Hash
        (["type",
         "message"]);

   end LogMessageParams_Scope;

   procedure Read_LogMessageParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LogMessageParams) is
      use LogMessageParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LogMessageParams_Map.Get_Index (Key) is
               when 1 =>  --  type
                  Read_MessageType (Handler, Value.a_type);
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
   end Read_LogMessageParams;

   package Definition_Result_Scope is
      package Definition_Result_Map is new Minimal_Perfect_Hash
        (["uri",
         "range",
         "alsKind"]);

   end Definition_Result_Scope;

   procedure Read_Definition_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Definition_Result) is
      use Definition_Result_Scope;
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
         if Handler.Is_Null_Value then
            Value :=
              (Kind   => LSP.Structures.Variant_3,
               others => <>);
         elsif Handler.Is_Start_Object then
            Handler.Read_Next;
            Value :=
              (Kind   => LSP.Structures.Variant_2,
               others => <>);
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Definition_Result_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  uri
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  range
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 3 =>  --  alsKind
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
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

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_Definition (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_DefinitionLink_Vector (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               null;  --  #null_value
               Handler.Read_Next;
         end case;
      end;
   end Read_Definition_Result;

end LSP.Inputs.Part_23;
