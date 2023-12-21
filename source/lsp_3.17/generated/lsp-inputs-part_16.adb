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

package body LSP.Inputs.Part_16 is

   package DocumentSymbol_Progress_Report_Scope is
      package DocumentSymbol_Progress_Report_Map is new Minimal_Perfect_Hash
        (["location",
         "containerName",
         "detail",
         "range",
         "selectionRange",
         "children",
         "alsIsDeclaration",
         "alsIsAdaProcedure",
         "alsVisibility"]);

   end DocumentSymbol_Progress_Report_Scope;

   procedure Read_DocumentSymbol_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol_Progress_Report) is
      use DocumentSymbol_Progress_Report_Scope;
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
                    DocumentSymbol_Progress_Report_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  location
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  containerName
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 3 =>  --  detail
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 4 =>  --  range
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 5 =>  --  selectionRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 6 =>  --  children
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 7 =>  --  alsIsDeclaration
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 8 =>  --  alsIsAdaProcedure
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 9 =>  --  alsVisibility
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
               Read_SymbolInformation_Vector (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_DocumentSymbol_Vector (Handler, Value.Variant_2);
         end case;
      end;
   end Read_DocumentSymbol_Progress_Report;

   procedure Read_DocumentSelector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSelector) is
      procedure Read_Virtual_String_Or_DocumentFilter
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_DocumentFilter);

      procedure Read_Virtual_String_Or_DocumentFilter
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_DocumentFilter) is
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
               Read_DocumentFilter (Handler, Value.DocumentFilter);
         end case;
      end Read_Virtual_String_Or_DocumentFilter;

   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DocumentSelector renames Value;
         Value : LSP.Structures.Virtual_String_Or_DocumentFilter;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_Virtual_String_Or_DocumentFilter (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_DocumentSelector;

   package CancelParams_Scope is
      package CancelParams_Map is new Minimal_Perfect_Hash (["id"]);

   end CancelParams_Scope;

   procedure Read_CancelParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CancelParams) is
      use CancelParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CancelParams_Map.Get_Index (Key) is
               when 1 =>  --  id
                  Read_Integer_Or_Virtual_String (Handler, Value.id);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CancelParams;

   package RelatedFullDocumentDiagnosticReport_Scope is
      package RelatedFullDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "resultId",
         "items",
         "relatedDocuments"]);

   end RelatedFullDocumentDiagnosticReport_Scope;

   procedure Read_RelatedFullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RelatedFullDocumentDiagnosticReport) is
      use RelatedFullDocumentDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RelatedFullDocumentDiagnosticReport_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: full
               when 2 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  items
                  Read_Diagnostic_Vector (Handler, Value.items);
               when 4 =>  --  relatedDocuments
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while not Handler.Is_End_Object loop
                     declare
                        Map   :
                          LSP.Structures
                            .relatedDocuments_OfDocumentDiagnosticReportPartialResult renames
                          Value.relatedDocuments;
                        Key   : LSP.Structures.DocumentUri;
                        Value :
                          LSP.Structures
                            .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item;
                     begin
                        Key := (Handler.Key_Name with null record);
                        Handler.Read_Next;
                        Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
                          (Handler, Value);
                        Map.Insert (Key, Value);
                     end;
                  end loop;

                  Handler.Read_Next;

               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RelatedFullDocumentDiagnosticReport;

   package VersionedTextDocumentIdentifier_Scope is
      package VersionedTextDocumentIdentifier_Map is new Minimal_Perfect_Hash
        (["uri",
         "version"]);

   end VersionedTextDocumentIdentifier_Scope;

   procedure Read_VersionedTextDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.VersionedTextDocumentIdentifier) is
      use VersionedTextDocumentIdentifier_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case VersionedTextDocumentIdentifier_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  version
                  Value.version :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_VersionedTextDocumentIdentifier;

   package clientInfo_Of_InitializeParams_Scope is
      package clientInfo_Of_InitializeParams_Map is new Minimal_Perfect_Hash
        (["name",
         "version"]);

   end clientInfo_Of_InitializeParams_Scope;

   procedure Read_clientInfo_Of_InitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.clientInfo_Of_InitializeParams) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            use clientInfo_Of_InitializeParams_Scope;
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case clientInfo_Of_InitializeParams_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
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
   end Read_clientInfo_Of_InitializeParams;

   package DefinitionClientCapabilities_Scope is
      package DefinitionClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "linkSupport"]);

   end DefinitionClientCapabilities_Scope;

   procedure Read_DefinitionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionClientCapabilities) is
      use DefinitionClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DefinitionClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  linkSupport
                  Value.linkSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.linkSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DefinitionClientCapabilities;

   package WorkspaceClientCapabilities_Scope is
      package WorkspaceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["applyEdit",
         "workspaceEdit",
         "didChangeConfiguration",
         "didChangeWatchedFiles",
         "symbol",
         "executeCommand",
         "workspaceFolders",
         "configuration",
         "semanticTokens",
         "codeLens",
         "fileOperations",
         "inlineValue",
         "inlayHint",
         "diagnostics"]);

   end WorkspaceClientCapabilities_Scope;

   procedure Read_WorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceClientCapabilities) is
      use WorkspaceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  applyEdit
                  Value.applyEdit       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.applyEdit.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  workspaceEdit
                  Value.workspaceEdit :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WorkspaceEditClientCapabilities
                    (Handler, Value.workspaceEdit.Value);
               when 3 =>  --  didChangeConfiguration
                  Value.didChangeConfiguration :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DidChangeConfigurationClientCapabilities
                    (Handler, Value.didChangeConfiguration.Value);
               when 4 =>  --  didChangeWatchedFiles
                  Value.didChangeWatchedFiles :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DidChangeWatchedFilesClientCapabilities
                    (Handler, Value.didChangeWatchedFiles.Value);
               when 5 =>  --  symbol
                  Value.symbol :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WorkspaceSymbolClientCapabilities
                    (Handler, Value.symbol.Value);
               when 6 =>  --  executeCommand
                  Value.executeCommand :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ExecuteCommandClientCapabilities
                    (Handler, Value.executeCommand.Value);
               when 7 =>  --  workspaceFolders
                  Value.workspaceFolders       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workspaceFolders.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 8 =>  --  configuration
                  Value.configuration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.configuration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 9 =>  --  semanticTokens
                  Value.semanticTokens :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SemanticTokensWorkspaceClientCapabilities
                    (Handler, Value.semanticTokens.Value);
               when 10 =>  --  codeLens
                  Value.codeLens :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeLensWorkspaceClientCapabilities
                    (Handler, Value.codeLens.Value);
               when 11 =>  --  fileOperations
                  Value.fileOperations :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationClientCapabilities
                    (Handler, Value.fileOperations.Value);
               when 12 =>  --  inlineValue
                  Value.inlineValue :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InlineValueWorkspaceClientCapabilities
                    (Handler, Value.inlineValue.Value);
               when 13 =>  --  inlayHint
                  Value.inlayHint :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InlayHintWorkspaceClientCapabilities
                    (Handler, Value.inlayHint.Value);
               when 14 =>  --  diagnostics
                  Value.diagnostics :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DiagnosticWorkspaceClientCapabilities
                    (Handler, Value.diagnostics.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceClientCapabilities;

   package CallHierarchyClientCapabilities_Scope is
      package CallHierarchyClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end CallHierarchyClientCapabilities_Scope;

   procedure Read_CallHierarchyClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyClientCapabilities) is
      use CallHierarchyClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyClientCapabilities_Map.Get_Index (Key) is
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
   end Read_CallHierarchyClientCapabilities;

   package Unregistration_Scope is
      package Unregistration_Map is new Minimal_Perfect_Hash
        (["id",
         "method"]);

   end Unregistration_Scope;

   procedure Read_Unregistration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Unregistration) is
      use Unregistration_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Unregistration_Map.Get_Index (Key) is
               when 1 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  method
                  Value.method.Clear;
                  Value.method.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Unregistration;

   package TextDocumentSyncClientCapabilities_Scope is
      package TextDocumentSyncClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "willSave",
         "willSaveWaitUntil",
         "didSave"]);

   end TextDocumentSyncClientCapabilities_Scope;

   procedure Read_TextDocumentSyncClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSyncClientCapabilities) is
      use TextDocumentSyncClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentSyncClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  willSave
                  Value.willSave       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willSave.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  willSaveWaitUntil
                  Value.willSaveWaitUntil       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willSaveWaitUntil.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  didSave
                  Value.didSave       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.didSave.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentSyncClientCapabilities;

   package ReferenceRegistrationOptions_Scope is
      package ReferenceRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end ReferenceRegistrationOptions_Scope;

   procedure Read_ReferenceRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceRegistrationOptions) is
      use ReferenceRegistrationOptions_Scope;
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
            case ReferenceRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_ReferenceRegistrationOptions;

end LSP.Inputs.Part_16;
