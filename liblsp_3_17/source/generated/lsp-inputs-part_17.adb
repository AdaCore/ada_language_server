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

package body LSP.Inputs.Part_17 is

   procedure Read_WorkspaceFolder_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFolder_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.WorkspaceFolder_Vector renames Value;
         Value : LSP.Structures.WorkspaceFolder;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_WorkspaceFolder (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_WorkspaceFolder_Vector;

   package SemanticTokenModifiers_Map is new Minimal_Perfect_Hash
     (["declaration",
      "definition",
      "readonly",
      "static",
      "deprecated",
      "abstract",
      "async",
      "modification",
      "documentation",
      "defaultLibrary"]);

   procedure Read_SemanticTokenModifiers
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SemanticTokenModifiers) is
   begin
      Value :=
        LSP.Enumerations.SemanticTokenModifiers'Val
          (SemanticTokenModifiers_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_SemanticTokenModifiers;

   package ConfigurationItem_Scope is
      package ConfigurationItem_Map is new Minimal_Perfect_Hash
        (["scopeUri",
         "section"]);

   end ConfigurationItem_Scope;

   procedure Read_ConfigurationItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ConfigurationItem) is
      use ConfigurationItem_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ConfigurationItem_Map.Get_Index (Key) is
               when 1 =>  --  scopeUri
                  Value.scopeUri.Clear;
                  Value.scopeUri.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  section
                  Value.section.Clear;
                  Value.section.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ConfigurationItem;

   procedure Read_Definition
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Definition) is
   begin
      declare
         Set   : LSP.Structures.Definition renames Value;
         Value : LSP.Structures.Location;
      begin
         Set.Clear;
         if Handler.Is_Start_Array then
            Handler.Read_Next;
            while not Handler.Is_End_Array loop
               Read_Location (Handler, Value);
               Set.Append (Value);
            end loop;
            Handler.Read_Next;

         else
            Read_Location (Handler, Value);
            Set.Append (Value);
         end if;
      end;

   end Read_Definition;

   package CallHierarchyOptions_Scope is
      package CallHierarchyOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end CallHierarchyOptions_Scope;

   procedure Read_CallHierarchyOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOptions) is
      use CallHierarchyOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
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
   end Read_CallHierarchyOptions;

   procedure Read_DefinitionLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionLink) renames
     Read_LocationLink;

   package DefinitionOptions_Scope is
      package DefinitionOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end DefinitionOptions_Scope;

   procedure Read_DefinitionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionOptions) is
      use DefinitionOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DefinitionOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
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
   end Read_DefinitionOptions;

   package FullDocumentDiagnosticReport_Scope is
      package FullDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "resultId",
         "items"]);

   end FullDocumentDiagnosticReport_Scope;

   procedure Read_FullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FullDocumentDiagnosticReport) is
      use FullDocumentDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FullDocumentDiagnosticReport_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: full
               when 2 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  items
                  Read_Diagnostic_Vector (Handler, Value.items);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FullDocumentDiagnosticReport;

   package ParameterInformation_Scope is
      package ParameterInformation_Map is new Minimal_Perfect_Hash
        (["label",
         "documentation"]);

   end ParameterInformation_Scope;

   procedure Read_ParameterInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ParameterInformation) is
      use ParameterInformation_Scope;
      procedure Read_Natural_Tuple
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Natural_Tuple);

      procedure Read_String_Or_Natural_Tuple
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.String_Or_Natural_Tuple);

      procedure Read_Natural_Tuple
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Natural_Tuple) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;
         for J in Value'Range loop
            Value (J) := Integer (Handler.Number_Value.Integer_Value);
            Handler.Read_Next;
         end loop;
         pragma Assert (Handler.Is_End_Array);
         Handler.Read_Next;
      end Read_Natural_Tuple;

      procedure Read_String_Or_Natural_Tuple
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.String_Or_Natural_Tuple) is
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
               Read_Natural_Tuple (Handler, Value.Natural_Tuple);
         end case;
      end Read_String_Or_Natural_Tuple;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ParameterInformation_Map.Get_Index (Key) is
               when 1 =>  --  label
                  Read_String_Or_Natural_Tuple (Handler, Value.label);
               when 2 =>  --  documentation
                  Value.documentation :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_MarkupContent
                    (Handler, Value.documentation.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ParameterInformation;

   package Symbol_Progress_Report_Scope is
      package Symbol_Progress_Report_Map is new Minimal_Perfect_Hash
        (["deprecated",
         "data"]);

   end Symbol_Progress_Report_Scope;

   procedure Read_Symbol_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Symbol_Progress_Report) is
      use Symbol_Progress_Report_Scope;
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
                    Symbol_Progress_Report_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  deprecated
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  data
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
               Read_WorkspaceSymbol_Vector (Handler, Value.Variant_2);
         end case;
      end;
   end Read_Symbol_Progress_Report;

   package CodeLensParams_Scope is
      package CodeLensParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument"]);

   end CodeLensParams_Scope;

   procedure Read_CodeLensParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensParams) is
      use CodeLensParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeLensParams_Map.Get_Index (Key) is
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
   end Read_CodeLensParams;

   package PreviousResultId_Scope is
      package PreviousResultId_Map is new Minimal_Perfect_Hash
        (["uri",
         "value"]);

   end PreviousResultId_Scope;

   procedure Read_PreviousResultId
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PreviousResultId) is
      use PreviousResultId_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case PreviousResultId_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  value
                  Value.value.Clear;
                  Value.value.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_PreviousResultId;

   package TextDocumentChangeRegistrationOptions_Scope is
      package TextDocumentChangeRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "syncKind"]);

   end TextDocumentChangeRegistrationOptions_Scope;

   procedure Read_TextDocumentChangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentChangeRegistrationOptions) is
      use TextDocumentChangeRegistrationOptions_Scope;
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
            case TextDocumentChangeRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.documentSelector);
               when 2 =>  --  syncKind
                  Read_TextDocumentSyncKind (Handler, Value.syncKind);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentChangeRegistrationOptions;

   package AlsDisplayMethodAncestryOnNavigationPolicy_Map is new Minimal_Perfect_Hash
     (["Never",
      "Usage_And_Abstract_Only",
      "Definition_Only",
      "Always"]);

   procedure Read_AlsDisplayMethodAncestryOnNavigationPolicy
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations
        .AlsDisplayMethodAncestryOnNavigationPolicy) is
   begin
      Value :=
        LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy'Val
          (AlsDisplayMethodAncestryOnNavigationPolicy_Map.Get_Index
             (Handler.String_Value) -
           1);
      Handler.Read_Next;
   end Read_AlsDisplayMethodAncestryOnNavigationPolicy;

   package DidCloseNotebookDocumentParams_Scope is
      package DidCloseNotebookDocumentParams_Map is new Minimal_Perfect_Hash
        (["notebookDocument",
         "cellTextDocuments"]);

   end DidCloseNotebookDocumentParams_Scope;

   procedure Read_DidCloseNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidCloseNotebookDocumentParams) is
      use DidCloseNotebookDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidCloseNotebookDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  notebookDocument
                  Read_NotebookDocumentIdentifier
                    (Handler, Value.notebookDocument);
               when 2 =>  --  cellTextDocuments
                  Read_TextDocumentIdentifier_Vector
                    (Handler, Value.cellTextDocuments);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidCloseNotebookDocumentParams;

   package DiagnosticServerCancellationData_Scope is
      package DiagnosticServerCancellationData_Map is new Minimal_Perfect_Hash
        (["retriggerRequest"]);

   end DiagnosticServerCancellationData_Scope;

   procedure Read_DiagnosticServerCancellationData
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticServerCancellationData) is
      use DiagnosticServerCancellationData_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DiagnosticServerCancellationData_Map.Get_Index (Key) is
               when 1 =>  --  retriggerRequest
                  Value.retriggerRequest := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DiagnosticServerCancellationData;

   package ShowMessageRequestParams_Scope is
      package ShowMessageRequestParams_Map is new Minimal_Perfect_Hash
        (["type",
         "message",
         "actions"]);

   end ShowMessageRequestParams_Scope;

   procedure Read_ShowMessageRequestParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowMessageRequestParams) is
      use ShowMessageRequestParams_Scope;
      procedure Read_MessageActionItem_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.MessageActionItem_Vector);

      procedure Read_MessageActionItem_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.MessageActionItem_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.MessageActionItem_Vector renames Value;
            Value : LSP.Structures.MessageActionItem;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_MessageActionItem (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_MessageActionItem_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ShowMessageRequestParams_Map.Get_Index (Key) is
               when 1 =>  --  type
                  Read_MessageType (Handler, Value.a_type);
               when 2 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  actions
                  Read_MessageActionItem_Vector (Handler, Value.actions);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ShowMessageRequestParams;

   package CompletionItemLabelDetails_Scope is
      package CompletionItemLabelDetails_Map is new Minimal_Perfect_Hash
        (["detail",
         "description"]);

   end CompletionItemLabelDetails_Scope;

   procedure Read_CompletionItemLabelDetails
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItemLabelDetails) is
      use CompletionItemLabelDetails_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionItemLabelDetails_Map.Get_Index (Key) is
               when 1 =>  --  detail
                  Value.detail.Clear;
                  Value.detail.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  description
                  Value.description.Clear;
                  Value.description.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionItemLabelDetails;

   package TextDocumentSyncOptions_Scope is
      package TextDocumentSyncOptions_Map is new Minimal_Perfect_Hash
        (["openClose",
         "change",
         "willSave",
         "willSaveWaitUntil",
         "save"]);

   end TextDocumentSyncOptions_Scope;

   procedure Read_TextDocumentSyncOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSyncOptions) is
      use TextDocumentSyncOptions_Scope;
      procedure Read_Boolean_Or_SaveOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_SaveOptions);

      procedure Read_Boolean_Or_SaveOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_SaveOptions) is
      begin
         if Handler.Is_Boolean_Value then
            Value :=
              (Is_Boolean => True,
               others     => <>);
         else
            Value :=
              (Is_Boolean => False,
               others     => <>);
         end if;

         case Value.Is_Boolean is
            when True =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
            when False =>
               Read_SaveOptions (Handler, Value.SaveOptions);
         end case;
      end Read_Boolean_Or_SaveOptions;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentSyncOptions_Map.Get_Index (Key) is
               when 1 =>  --  openClose
                  Value.openClose       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.openClose.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  change
                  Value.change :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TextDocumentSyncKind (Handler, Value.change.Value);
               when 3 =>  --  willSave
                  Value.willSave       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willSave.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  willSaveWaitUntil
                  Value.willSaveWaitUntil       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willSaveWaitUntil.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  save
                  Value.save :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_SaveOptions (Handler, Value.save.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentSyncOptions;

end LSP.Inputs.Part_17;
