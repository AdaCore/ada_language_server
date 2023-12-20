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

package body LSP.Inputs.Part_32 is

   package SemanticTokenTypes_Map is new Minimal_Perfect_Hash
     (["namespace",
      "type",
      "class",
      "enum",
      "interface",
      "struct",
      "typeParameter",
      "parameter",
      "variable",
      "property",
      "enumMember",
      "event",
      "function",
      "method",
      "macro",
      "keyword",
      "modifier",
      "comment",
      "string",
      "number",
      "regexp",
      "operator",
      "decorator"]);

   procedure Read_SemanticTokenTypes
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SemanticTokenTypes) is
   begin
      Value :=
        LSP.Enumerations.SemanticTokenTypes'Val
          (SemanticTokenTypes_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_SemanticTokenTypes;

   package FileEvent_Scope is
      package FileEvent_Map is new Minimal_Perfect_Hash
        (["uri",
         "type"]);

   end FileEvent_Scope;

   procedure Read_FileEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileEvent) is
      use FileEvent_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileEvent_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  type
                  Read_FileChangeType (Handler, Value.a_type);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileEvent;

   package SignatureHelpClientCapabilities_Scope is
      package SignatureHelpClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "signatureInformation",
         "contextSupport"]);

      package signatureInformation_OfSignatureHelpClientCapabilities_Scope is
         package signatureInformation_OfSignatureHelpClientCapabilities_Map is new Minimal_Perfect_Hash
           (["documentationFormat",
            "parameterInformation",
            "activeParameterSupport"]);

      end signatureInformation_OfSignatureHelpClientCapabilities_Scope;

      package parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities_Scope
      is
         package parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities_Map is new Minimal_Perfect_Hash
           (["labelOffsetSupport"]);

      end parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities_Scope;

   end SignatureHelpClientCapabilities_Scope;

   procedure Read_SignatureHelpClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpClientCapabilities) is
      use SignatureHelpClientCapabilities_Scope;
      procedure Read_signatureInformation_OfSignatureHelpClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .signatureInformation_OfSignatureHelpClientCapabilities);

      procedure Read_parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities);

      procedure Read_signatureInformation_OfSignatureHelpClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .signatureInformation_OfSignatureHelpClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 signatureInformation_OfSignatureHelpClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case signatureInformation_OfSignatureHelpClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  documentationFormat
                     Read_MarkupKind_Vector
                       (Handler, Value.documentationFormat);
                  when 2 =>  --  parameterInformation
                     Value.parameterInformation :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities
                       (Handler, Value.parameterInformation.Value);
                  when 3 =>  --  activeParameterSupport
                     Value.activeParameterSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.activeParameterSupport.Value :=
                       Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_signatureInformation_OfSignatureHelpClientCapabilities;

      procedure Read_parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  labelOffsetSupport
                     Value.labelOffsetSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.labelOffsetSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureHelpClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  signatureInformation
                  Value.signatureInformation :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_signatureInformation_OfSignatureHelpClientCapabilities
                    (Handler, Value.signatureInformation.Value);
               when 3 =>  --  contextSupport
                  Value.contextSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.contextSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureHelpClientCapabilities;

   procedure Read_LSPObject
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPObject) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      LSP.Input_Tools.Read_LSPAny_Class (Handler, Value);

   end Read_LSPObject;

   package WorkspaceUnchangedDocumentDiagnosticReport_Scope is
      package WorkspaceUnchangedDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "resultId",
         "uri",
         "version"]);

   end WorkspaceUnchangedDocumentDiagnosticReport_Scope;

   procedure Read_WorkspaceUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.WorkspaceUnchangedDocumentDiagnosticReport) is
      use WorkspaceUnchangedDocumentDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceUnchangedDocumentDiagnosticReport_Map.Get_Index
              (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: unchanged
               when 2 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 4 =>  --  version
                  Read_Integer_Or_Null (Handler, Value.version);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceUnchangedDocumentDiagnosticReport;

   package SemanticTokensDelta_Scope is
      package SemanticTokensDelta_Map is new Minimal_Perfect_Hash
        (["resultId",
         "edits"]);

   end SemanticTokensDelta_Scope;

   procedure Read_SemanticTokensDelta
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensDelta) is
      use SemanticTokensDelta_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensDelta_Map.Get_Index (Key) is
               when 1 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  edits
                  Read_SemanticTokensEdit_Vector (Handler, Value.edits);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensDelta;

   package TypeHierarchyItem_Scope is
      package TypeHierarchyItem_Map is new Minimal_Perfect_Hash
        (["name",
         "kind",
         "tags",
         "detail",
         "uri",
         "range",
         "selectionRange",
         "data"]);

   end TypeHierarchyItem_Scope;

   procedure Read_TypeHierarchyItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyItem) is
      use TypeHierarchyItem_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchyItem_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  kind
                  Read_SymbolKind (Handler, Value.kind);
               when 3 =>  --  tags
                  Read_SymbolTag_Set (Handler, Value.tags);
               when 4 =>  --  detail
                  Value.detail.Clear;
                  Value.detail.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 6 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 7 =>  --  selectionRange
                  Read_A_Range (Handler, Value.selectionRange);
               when 8 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeHierarchyItem;

   procedure Read_TypeHierarchyItem_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_TypeHierarchyItem_Vector (Handler, Value);
      end if;
   end Read_TypeHierarchyItem_Vector_Or_Null;

   package WorkspaceFoldersChangeEvent_Scope is
      package WorkspaceFoldersChangeEvent_Map is new Minimal_Perfect_Hash
        (["added",
         "removed"]);

   end WorkspaceFoldersChangeEvent_Scope;

   procedure Read_WorkspaceFoldersChangeEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFoldersChangeEvent) is
      use WorkspaceFoldersChangeEvent_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceFoldersChangeEvent_Map.Get_Index (Key) is
               when 1 =>  --  added
                  Read_WorkspaceFolder_Vector (Handler, Value.added);
               when 2 =>  --  removed
                  Read_WorkspaceFolder_Vector (Handler, Value.removed);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceFoldersChangeEvent;

   package SignatureHelpContext_Scope is
      package SignatureHelpContext_Map is new Minimal_Perfect_Hash
        (["triggerKind",
         "triggerCharacter",
         "isRetrigger",
         "activeSignatureHelp"]);

   end SignatureHelpContext_Scope;

   procedure Read_SignatureHelpContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpContext) is
      use SignatureHelpContext_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureHelpContext_Map.Get_Index (Key) is
               when 1 =>  --  triggerKind
                  Read_SignatureHelpTriggerKind (Handler, Value.triggerKind);
               when 2 =>  --  triggerCharacter
                  Value.triggerCharacter.Clear;
                  Value.triggerCharacter.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  isRetrigger
                  Value.isRetrigger := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  activeSignatureHelp
                  Value.activeSignatureHelp :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SignatureHelp
                    (Handler, Value.activeSignatureHelp.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureHelpContext;

   package SymbolInformation_Scope is
      package SymbolInformation_Map is new Minimal_Perfect_Hash
        (["name",
         "kind",
         "tags",
         "containerName",
         "deprecated",
         "location"]);

   end SymbolInformation_Scope;

   procedure Read_SymbolInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolInformation) is
      use SymbolInformation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SymbolInformation_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  kind
                  Read_SymbolKind (Handler, Value.kind);
               when 3 =>  --  tags
                  Read_SymbolTag_Set (Handler, Value.tags);
               when 4 =>  --  containerName
                  Value.containerName.Clear;
                  Value.containerName.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  deprecated
                  Value.deprecated       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.deprecated.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  location
                  Read_Location (Handler, Value.location);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SymbolInformation;

   package HoverParams_Scope is
      package HoverParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken"]);

   end HoverParams_Scope;

   procedure Read_HoverParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverParams) is
      use HoverParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case HoverParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_HoverParams;

end LSP.Inputs.Part_32;
