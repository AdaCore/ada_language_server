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

package body LSP.Inputs.Part_27 is

   package ClientCapabilities_Scope is
      package ClientCapabilities_Map is new Minimal_Perfect_Hash
        (["workspace",
         "textDocument",
         "notebookDocument",
         "window",
         "general",
         "experimental"]);

   end ClientCapabilities_Scope;

   procedure Read_ClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ClientCapabilities) is
      use ClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  workspace
                  Value.workspace :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WorkspaceClientCapabilities
                    (Handler, Value.workspace.Value);
               when 2 =>  --  textDocument
                  Value.textDocument :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TextDocumentClientCapabilities
                    (Handler, Value.textDocument.Value);
               when 3 =>  --  notebookDocument
                  Value.notebookDocument :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_NotebookDocumentClientCapabilities
                    (Handler, Value.notebookDocument.Value);
               when 4 =>  --  window
                  Value.window :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WindowClientCapabilities (Handler, Value.window.Value);
               when 5 =>  --  general
                  Value.general :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_GeneralClientCapabilities
                    (Handler, Value.general.Value);
               when 6 =>  --  experimental
                  Read_LSPAny (Handler, Value.experimental);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ClientCapabilities;

   package AnnotatedTextEdit_Scope is
      package AnnotatedTextEdit_Map is new Minimal_Perfect_Hash
        (["range",
         "newText",
         "annotationId"]);

   end AnnotatedTextEdit_Scope;

   procedure Read_AnnotatedTextEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.AnnotatedTextEdit) is
      use AnnotatedTextEdit_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case AnnotatedTextEdit_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  newText
                  Value.newText.Clear;
                  Value.newText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  annotationId
                  Read_ChangeAnnotationIdentifier
                    (Handler, Value.annotationId);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_AnnotatedTextEdit;

   procedure Read_CodeLens_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLens_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CodeLens_Vector renames Value;
         Value : LSP.Structures.CodeLens;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_CodeLens (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_CodeLens_Vector;

   package SelectionRange_Scope is
      package SelectionRange_Map is new Minimal_Perfect_Hash
        (["range",
         "parent"]);

   end SelectionRange_Scope;

   procedure Read_SelectionRange
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRange) is
      use SelectionRange_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SelectionRange_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  parent
                  declare
                     Value_parent : LSP.Structures.SelectionRange;
                  begin
                     Read_SelectionRange (Handler, Value_parent);
                     Value.parent.Set (Value_parent);
                  end;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SelectionRange;

   package RenameClientCapabilities_Scope is
      package RenameClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "prepareSupport",
         "prepareSupportDefaultBehavior",
         "honorsChangeAnnotations"]);

   end RenameClientCapabilities_Scope;

   procedure Read_RenameClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameClientCapabilities) is
      use RenameClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  prepareSupport
                  Value.prepareSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.prepareSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  prepareSupportDefaultBehavior
                  Value.prepareSupportDefaultBehavior :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_PrepareSupportDefaultBehavior
                    (Handler, Value.prepareSupportDefaultBehavior.Value);
               when 4 =>  --  honorsChangeAnnotations
                  Value.honorsChangeAnnotations       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.honorsChangeAnnotations.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameClientCapabilities;

   procedure Read_LSPAny_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPAny_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_LSPAny (Handler, Value.Value);
      end if;
   end Read_LSPAny_Or_Null;

   package SelectionRangeOptions_Scope is
      package SelectionRangeOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end SelectionRangeOptions_Scope;

   procedure Read_SelectionRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeOptions) is
      use SelectionRangeOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SelectionRangeOptions_Map.Get_Index (Key) is
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
   end Read_SelectionRangeOptions;

   procedure Read_Boolean_Or_Any
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Boolean_Or_Any) is
   begin
      LSP.Input_Tools.Read_LSPAny_Class (Handler, Value);
   end Read_Boolean_Or_Any;

   package SetTraceParams_Scope is
      package SetTraceParams_Map is new Minimal_Perfect_Hash (["value"]);

   end SetTraceParams_Scope;

   procedure Read_SetTraceParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SetTraceParams) is
      use SetTraceParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SetTraceParams_Map.Get_Index (Key) is
               when 1 =>  --  value
                  Read_TraceValues (Handler, Value.value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SetTraceParams;

   package BaseSymbolInformation_Scope is
      package BaseSymbolInformation_Map is new Minimal_Perfect_Hash
        (["name",
         "kind",
         "tags",
         "containerName"]);

   end BaseSymbolInformation_Scope;

   procedure Read_BaseSymbolInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.BaseSymbolInformation) is
      use BaseSymbolInformation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case BaseSymbolInformation_Map.Get_Index (Key) is
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
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_BaseSymbolInformation;

   package TypeHierarchyOptions_Scope is
      package TypeHierarchyOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end TypeHierarchyOptions_Scope;

   procedure Read_TypeHierarchyOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyOptions) is
      use TypeHierarchyOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchyOptions_Map.Get_Index (Key) is
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
   end Read_TypeHierarchyOptions;

   package DocumentLink_Scope is
      package DocumentLink_Map is new Minimal_Perfect_Hash
        (["range",
         "target",
         "tooltip",
         "data"]);

   end DocumentLink_Scope;

   procedure Read_DocumentLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLink) is
      use DocumentLink_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentLink_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  target
                  Value.target.Clear;
                  Value.target.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  tooltip
                  Value.tooltip.Clear;
                  Value.tooltip.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 4 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentLink;

   procedure Read_WorkspaceEdit_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceEdit_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_WorkspaceEdit (Handler, Value.Value);
      end if;
   end Read_WorkspaceEdit_Or_Null;

   procedure Read_DocumentHighlight_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlight_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_DocumentHighlight_Vector (Handler, Value);
      end if;
   end Read_DocumentHighlight_Vector_Or_Null;

   package DocumentLinkOptions_Scope is
      package DocumentLinkOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "resolveProvider"]);

   end DocumentLinkOptions_Scope;

   procedure Read_DocumentLinkOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkOptions) is
      use DocumentLinkOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentLinkOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resolveProvider
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
   end Read_DocumentLinkOptions;

   package InlineValueText_Scope is
      package InlineValueText_Map is new Minimal_Perfect_Hash
        (["range",
         "text"]);

   end InlineValueText_Scope;

   procedure Read_InlineValueText
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueText) is
      use InlineValueText_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueText_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  text
                  Value.text.Clear;
                  Value.text.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueText;

   package UnchangedDocumentDiagnosticReport_Scope is
      package UnchangedDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "resultId"]);

   end UnchangedDocumentDiagnosticReport_Scope;

   procedure Read_UnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.UnchangedDocumentDiagnosticReport) is
      use UnchangedDocumentDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case UnchangedDocumentDiagnosticReport_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: unchanged
               when 2 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_UnchangedDocumentDiagnosticReport;

   package ResourceOperation_Scope is
      package ResourceOperation_Map is new Minimal_Perfect_Hash
        (["kind",
         "annotationId"]);

   end ResourceOperation_Scope;

   procedure Read_ResourceOperation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ResourceOperation) is
      use ResourceOperation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ResourceOperation_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Value.kind.Clear;
                  Value.kind.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  annotationId
                  Value.annotationId :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ChangeAnnotationIdentifier
                    (Handler, Value.annotationId.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ResourceOperation;

   package CodeActionParams_Scope is
      package CodeActionParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "range",
         "context"]);

   end CodeActionParams_Scope;

   procedure Read_CodeActionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionParams) is
      use CodeActionParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeActionParams_Map.Get_Index (Key) is
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
               when 5 =>  --  context
                  Read_CodeActionContext (Handler, Value.context);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeActionParams;

end LSP.Inputs.Part_27;
