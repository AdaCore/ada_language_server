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

package body LSP.Inputs.Part_18 is

   package InitializeParams_Scope is
      package InitializeParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "processId",
         "clientInfo",
         "locale",
         "rootPath",
         "rootUri",
         "capabilities",
         "initializationOptions",
         "trace",
         "workspaceFolders"]);

      package trace_Of_InitializeParams_Scope is
         package trace_Of_InitializeParams_Map is new Minimal_Perfect_Hash
           (["off",
            "messages",
            "compact",
            "verbose"]);

      end trace_Of_InitializeParams_Scope;

   end InitializeParams_Scope;

   procedure Read_InitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializeParams) is
      use InitializeParams_Scope;
      procedure Read_trace_Of_InitializeParams
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.trace_Of_InitializeParams);

      procedure Read_Virtual_String_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_Null);

      procedure Read_DocumentUri_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentUri_Or_Null);

      procedure Read_trace_Of_InitializeParams
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.trace_Of_InitializeParams) is
         use trace_Of_InitializeParams_Scope;
      begin
         Value :=
           LSP.Structures.trace_Of_InitializeParams'Val
             (trace_Of_InitializeParams_Map.Get_Index (Handler.String_Value) -
              1);
         Handler.Read_Next;
      end Read_trace_Of_InitializeParams;

      procedure Read_Virtual_String_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Value.Value.Clear;
            Value.Value.Append (Handler.String_Value);
            Handler.Read_Next;
         end if;
      end Read_Virtual_String_Or_Null;

      procedure Read_DocumentUri_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentUri_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value       :=
              (Is_Null => False,
               Value   => <>);
            Value.Value := (Handler.String_Value with null record);
            Handler.Read_Next;
         end if;
      end Read_DocumentUri_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InitializeParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  processId
                  Read_Integer_Or_Null (Handler, Value.processId);
               when 3 =>  --  clientInfo
                  Value.clientInfo :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_clientInfo_Of_InitializeParams
                    (Handler, Value.clientInfo.Value);
               when 4 =>  --  locale
                  Value.locale.Clear;
                  Value.locale.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  rootPath
                  Value.rootPath :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_Null (Handler, Value.rootPath.Value);
               when 6 =>  --  rootUri
                  Read_DocumentUri_Or_Null (Handler, Value.rootUri);
               when 7 =>  --  capabilities
                  Read_ClientCapabilities (Handler, Value.capabilities);
               when 8 =>  --  initializationOptions
                  Read_LSPAny (Handler, Value.initializationOptions);
               when 9 =>  --  trace
                  Value.trace :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_trace_Of_InitializeParams (Handler, Value.trace.Value);
               when 10 =>  --  workspaceFolders
                  Value.Parent.workspaceFolders :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WorkspaceFolder_Vector_Or_Null
                    (Handler, Value.Parent.workspaceFolders.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InitializeParams;

   package DocumentDiagnosticParams_Scope is
      package DocumentDiagnosticParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "identifier",
         "previousResultId"]);

   end DocumentDiagnosticParams_Scope;

   procedure Read_DocumentDiagnosticParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentDiagnosticParams) is
      use DocumentDiagnosticParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentDiagnosticParams_Map.Get_Index (Key) is
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
               when 4 =>  --  identifier
                  Value.identifier.Clear;
                  Value.identifier.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  previousResultId
                  Value.previousResultId.Clear;
                  Value.previousResultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentDiagnosticParams;

   package ShowMessageParams_Scope is
      package ShowMessageParams_Map is new Minimal_Perfect_Hash
        (["type",
         "message"]);

   end ShowMessageParams_Scope;

   procedure Read_ShowMessageParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowMessageParams) is
      use ShowMessageParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ShowMessageParams_Map.Get_Index (Key) is
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
   end Read_ShowMessageParams;

   procedure Read_DiagnosticSeverity
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DiagnosticSeverity) is
   begin
      Value :=
        LSP.Enumerations.DiagnosticSeverity'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_DiagnosticSeverity;

   package ReferenceContext_Scope is
      package ReferenceContext_Map is new Minimal_Perfect_Hash
        (["includeDeclaration"]);

   end ReferenceContext_Scope;

   procedure Read_ReferenceContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceContext) is
      use ReferenceContext_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ReferenceContext_Map.Get_Index (Key) is
               when 1 =>  --  includeDeclaration
                  Value.includeDeclaration := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ReferenceContext;

   package An_InitializeParams_Scope is
      package An_InitializeParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "processId",
         "clientInfo",
         "locale",
         "rootPath",
         "rootUri",
         "capabilities",
         "initializationOptions",
         "trace"]);

      package trace_Of_InitializeParams_Scope is
         package trace_Of_InitializeParams_Map is new Minimal_Perfect_Hash
           (["off",
            "messages",
            "compact",
            "verbose"]);

      end trace_Of_InitializeParams_Scope;

   end An_InitializeParams_Scope;

   procedure Read_An_InitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.An_InitializeParams) is
      use An_InitializeParams_Scope;
      procedure Read_trace_Of_InitializeParams
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.trace_Of_InitializeParams);

      procedure Read_Virtual_String_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_Null);

      procedure Read_DocumentUri_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentUri_Or_Null);

      procedure Read_trace_Of_InitializeParams
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.trace_Of_InitializeParams) is
         use trace_Of_InitializeParams_Scope;
      begin
         Value :=
           LSP.Structures.trace_Of_InitializeParams'Val
             (trace_Of_InitializeParams_Map.Get_Index (Handler.String_Value) -
              1);
         Handler.Read_Next;
      end Read_trace_Of_InitializeParams;

      procedure Read_Virtual_String_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Value.Value.Clear;
            Value.Value.Append (Handler.String_Value);
            Handler.Read_Next;
         end if;
      end Read_Virtual_String_Or_Null;

      procedure Read_DocumentUri_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentUri_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value       :=
              (Is_Null => False,
               Value   => <>);
            Value.Value := (Handler.String_Value with null record);
            Handler.Read_Next;
         end if;
      end Read_DocumentUri_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case An_InitializeParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  processId
                  Read_Integer_Or_Null (Handler, Value.processId);
               when 3 =>  --  clientInfo
                  Value.clientInfo :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_clientInfo_Of_InitializeParams
                    (Handler, Value.clientInfo.Value);
               when 4 =>  --  locale
                  Value.locale.Clear;
                  Value.locale.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  rootPath
                  Value.rootPath :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_Null (Handler, Value.rootPath.Value);
               when 6 =>  --  rootUri
                  Read_DocumentUri_Or_Null (Handler, Value.rootUri);
               when 7 =>  --  capabilities
                  Read_ClientCapabilities (Handler, Value.capabilities);
               when 8 =>  --  initializationOptions
                  Read_LSPAny (Handler, Value.initializationOptions);
               when 9 =>  --  trace
                  Value.trace :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_trace_Of_InitializeParams (Handler, Value.trace.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_An_InitializeParams;

   package DefinitionRegistrationOptions_Scope is
      package DefinitionRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end DefinitionRegistrationOptions_Scope;

   procedure Read_DefinitionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionRegistrationOptions) is
      use DefinitionRegistrationOptions_Scope;
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
            case DefinitionRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DefinitionRegistrationOptions;

   package ExecuteCommandParams_Scope is
      package ExecuteCommandParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "command",
         "arguments"]);

   end ExecuteCommandParams_Scope;

   procedure Read_ExecuteCommandParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandParams) is
      use ExecuteCommandParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ExecuteCommandParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  command
                  Value.command.Clear;
                  Value.command.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  arguments
                  Read_LSPAny_Vector (Handler, Value.arguments);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ExecuteCommandParams;

   procedure Read_SemanticTokens_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokens_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_SemanticTokens (Handler, Value.Value);
      end if;
   end Read_SemanticTokens_Or_Null;

   package CallHierarchyRegistrationOptions_Scope is
      package CallHierarchyRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end CallHierarchyRegistrationOptions_Scope;

   procedure Read_CallHierarchyRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyRegistrationOptions) is
      use CallHierarchyRegistrationOptions_Scope;
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
            case CallHierarchyRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyRegistrationOptions;

   package ReferenceClientCapabilities_Scope is
      package ReferenceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end ReferenceClientCapabilities_Scope;

   procedure Read_ReferenceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceClientCapabilities) is
      use ReferenceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ReferenceClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ReferenceClientCapabilities;

   package OptionalVersionedTextDocumentIdentifier_Scope is
      package OptionalVersionedTextDocumentIdentifier_Map is new Minimal_Perfect_Hash
        (["uri",
         "version"]);

   end OptionalVersionedTextDocumentIdentifier_Scope;

   procedure Read_OptionalVersionedTextDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.OptionalVersionedTextDocumentIdentifier) is
      use OptionalVersionedTextDocumentIdentifier_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case OptionalVersionedTextDocumentIdentifier_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  version
                  Read_Integer_Or_Null (Handler, Value.version);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_OptionalVersionedTextDocumentIdentifier;

end LSP.Inputs.Part_18;
