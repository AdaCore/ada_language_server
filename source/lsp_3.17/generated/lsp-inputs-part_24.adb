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

package body LSP.Inputs.Part_24 is

   package DidSaveNotebookDocumentParams_Scope is
      package DidSaveNotebookDocumentParams_Map is new Minimal_Perfect_Hash
        (["notebookDocument"]);

   end DidSaveNotebookDocumentParams_Scope;

   procedure Read_DidSaveNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidSaveNotebookDocumentParams) is
      use DidSaveNotebookDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidSaveNotebookDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  notebookDocument
                  Read_NotebookDocumentIdentifier
                    (Handler, Value.notebookDocument);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidSaveNotebookDocumentParams;

   package CodeActionRegistrationOptions_Scope is
      package CodeActionRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "codeActionKinds",
         "resolveProvider"]);

   end CodeActionRegistrationOptions_Scope;

   procedure Read_CodeActionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionRegistrationOptions) is
      use CodeActionRegistrationOptions_Scope;
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
            case CodeActionRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  codeActionKinds
                  Read_CodeActionKind_Set (Handler, Value.codeActionKinds);
               when 4 =>  --  resolveProvider
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
   end Read_CodeActionRegistrationOptions;

   package InlineValue_Scope is
      package InlineValue_Map is new Minimal_Perfect_Hash
        (["text",
         "variableName",
         "caseSensitiveLookup",
         "expression"]);

   end InlineValue_Scope;

   procedure Read_InlineValue
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValue) is
      use InlineValue_Scope;
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
                  Index : constant Natural := InlineValue_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  text
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  variableName
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 3 =>  --  caseSensitiveLookup
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 4 =>  --  expression
                        Value :=
                          (Kind   => LSP.Structures.Variant_3,
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
               Read_InlineValueText (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_InlineValueVariableLookup (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Read_InlineValueEvaluatableExpression
                 (Handler, Value.Variant_3);
         end case;
      end;
   end Read_InlineValue;

   package TypeDefinitionClientCapabilities_Scope is
      package TypeDefinitionClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "linkSupport"]);

   end TypeDefinitionClientCapabilities_Scope;

   procedure Read_TypeDefinitionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionClientCapabilities) is
      use TypeDefinitionClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeDefinitionClientCapabilities_Map.Get_Index (Key) is
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
   end Read_TypeDefinitionClientCapabilities;

   package CreateFileOptions_Scope is
      package CreateFileOptions_Map is new Minimal_Perfect_Hash
        (["overwrite",
         "ignoreIfExists"]);

   end CreateFileOptions_Scope;

   procedure Read_CreateFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFileOptions) is
      use CreateFileOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CreateFileOptions_Map.Get_Index (Key) is
               when 1 =>  --  overwrite
                  Value.overwrite       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.overwrite.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  ignoreIfExists
                  Value.ignoreIfExists       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.ignoreIfExists.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CreateFileOptions;

   procedure Read_InlineValue_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValue_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_InlineValue_Vector (Handler, Value);
      end if;
   end Read_InlineValue_Vector_Or_Null;

   package PublishDiagnosticsClientCapabilities_Scope is
      package PublishDiagnosticsClientCapabilities_Map is new Minimal_Perfect_Hash
        (["relatedInformation",
         "tagSupport",
         "versionSupport",
         "codeDescriptionSupport",
         "dataSupport"]);

      package tagSupport_OfPublishDiagnosticsClientCapabilities_Scope is
         package tagSupport_OfPublishDiagnosticsClientCapabilities_Map is new Minimal_Perfect_Hash
           (["valueSet"]);

      end tagSupport_OfPublishDiagnosticsClientCapabilities_Scope;

   end PublishDiagnosticsClientCapabilities_Scope;

   procedure Read_PublishDiagnosticsClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PublishDiagnosticsClientCapabilities) is
      use PublishDiagnosticsClientCapabilities_Scope;
      procedure Read_tagSupport_OfPublishDiagnosticsClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .tagSupport_OfPublishDiagnosticsClientCapabilities);

      procedure Read_tagSupport_OfPublishDiagnosticsClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .tagSupport_OfPublishDiagnosticsClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use tagSupport_OfPublishDiagnosticsClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case tagSupport_OfPublishDiagnosticsClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  valueSet
                     Read_DiagnosticTag_Set (Handler, Value.valueSet);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_tagSupport_OfPublishDiagnosticsClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case PublishDiagnosticsClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  relatedInformation
                  Value.relatedInformation       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.relatedInformation.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  tagSupport
                  Value.tagSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_tagSupport_OfPublishDiagnosticsClientCapabilities
                    (Handler, Value.tagSupport.Value);
               when 3 =>  --  versionSupport
                  Value.versionSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.versionSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  codeDescriptionSupport
                  Value.codeDescriptionSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.codeDescriptionSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  dataSupport
                  Value.dataSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dataSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_PublishDiagnosticsClientCapabilities;

   package SemanticTokensOptions_Scope is
      package SemanticTokensOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "legend",
         "range",
         "full"]);

   end SemanticTokensOptions_Scope;

   procedure Read_SemanticTokensOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensOptions) is
      use SemanticTokensOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  legend
                  Read_SemanticTokensLegend (Handler, Value.legend);
               when 3 =>  --  range
                  Value.a_range :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_Any (Handler, Value.a_range.Value);
               when 4 =>  --  full
                  Value.full :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_Something (Handler, Value.full.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensOptions;

   package FoldingRangeParams_Scope is
      package FoldingRangeParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument"]);

   end FoldingRangeParams_Scope;

   procedure Read_FoldingRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeParams) is
      use FoldingRangeParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FoldingRangeParams_Map.Get_Index (Key) is
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
   end Read_FoldingRangeParams;

   package TextDocumentSaveRegistrationOptions_Scope is
      package TextDocumentSaveRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "includeText"]);

   end TextDocumentSaveRegistrationOptions_Scope;

   procedure Read_TextDocumentSaveRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSaveRegistrationOptions) is
      use TextDocumentSaveRegistrationOptions_Scope;
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
            case TextDocumentSaveRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  includeText
                  Value.includeText       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.includeText.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentSaveRegistrationOptions;

end LSP.Inputs.Part_24;
