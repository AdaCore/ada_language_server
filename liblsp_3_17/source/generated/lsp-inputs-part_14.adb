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

package body LSP.Inputs.Part_14 is

   procedure Read_FileChangeType
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FileChangeType) is
   begin
      Value :=
        LSP.Enumerations.FileChangeType'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_FileChangeType;

   package SignatureInformation_Scope is
      package SignatureInformation_Map is new Minimal_Perfect_Hash
        (["label",
         "documentation",
         "parameters",
         "activeParameter"]);

   end SignatureInformation_Scope;

   procedure Read_SignatureInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureInformation) is
      use SignatureInformation_Scope;
      procedure Read_ParameterInformation_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.ParameterInformation_Vector);

      procedure Read_ParameterInformation_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.ParameterInformation_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.ParameterInformation_Vector renames Value;
            Value : LSP.Structures.ParameterInformation;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_ParameterInformation (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_ParameterInformation_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureInformation_Map.Get_Index (Key) is
               when 1 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  documentation
                  Value.documentation :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_MarkupContent
                    (Handler, Value.documentation.Value);
               when 3 =>  --  parameters
                  Read_ParameterInformation_Vector (Handler, Value.parameters);
               when 4 =>  --  activeParameter
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
   end Read_SignatureInformation;

   package SemanticTokensWorkspaceClientCapabilities_Scope is
      package SemanticTokensWorkspaceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["refreshSupport"]);

   end SemanticTokensWorkspaceClientCapabilities_Scope;

   procedure Read_SemanticTokensWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.SemanticTokensWorkspaceClientCapabilities) is
      use SemanticTokensWorkspaceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensWorkspaceClientCapabilities_Map.Get_Index
              (Key) is
               when 1 =>  --  refreshSupport
                  Value.refreshSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.refreshSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensWorkspaceClientCapabilities;

   package PrepareRenameResult_Scope is
      package PrepareRenameResult_Map is new Minimal_Perfect_Hash
        (["start",
         "end",
         "range",
         "placeholder",
         "defaultBehavior"]);

      package PrepareRenameResult_2_Scope is
         package PrepareRenameResult_2_Map is new Minimal_Perfect_Hash
           (["range",
            "placeholder"]);

      end PrepareRenameResult_2_Scope;

      package PrepareRenameResult_3_Scope is
         package PrepareRenameResult_3_Map is new Minimal_Perfect_Hash
           (["defaultBehavior"]);

      end PrepareRenameResult_3_Scope;

   end PrepareRenameResult_Scope;

   procedure Read_PrepareRenameResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PrepareRenameResult) is
      use PrepareRenameResult_Scope;
      procedure Read_PrepareRenameResult_2
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PrepareRenameResult_2);

      procedure Read_PrepareRenameResult_3
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PrepareRenameResult_3);

      procedure Read_PrepareRenameResult_2
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PrepareRenameResult_2) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use PrepareRenameResult_2_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case PrepareRenameResult_2_Map.Get_Index (Key) is
                  when 1 =>  --  range
                     Read_A_Range (Handler, Value.a_range);
                  when 2 =>  --  placeholder
                     Value.placeholder.Clear;
                     Value.placeholder.Append (Handler.String_Value);
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_PrepareRenameResult_2;

      procedure Read_PrepareRenameResult_3
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PrepareRenameResult_3) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use PrepareRenameResult_3_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case PrepareRenameResult_3_Map.Get_Index (Key) is
                  when 1 =>  --  defaultBehavior
                     Value.defaultBehavior := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_PrepareRenameResult_3;

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
                    PrepareRenameResult_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  start
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  end
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 3 =>  --  range
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 4 =>  --  placeholder
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 5 =>  --  defaultBehavior
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
               Read_A_Range (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_PrepareRenameResult_2 (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Read_PrepareRenameResult_3 (Handler, Value.Variant_3);
         end case;
      end;
   end Read_PrepareRenameResult;

   package DocumentSymbolRegistrationOptions_Scope is
      package DocumentSymbolRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "label"]);

   end DocumentSymbolRegistrationOptions_Scope;

   procedure Read_DocumentSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolRegistrationOptions) is
      use DocumentSymbolRegistrationOptions_Scope;
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
            case DocumentSymbolRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentSymbolRegistrationOptions;

   package MarkdownClientCapabilities_Scope is
      package MarkdownClientCapabilities_Map is new Minimal_Perfect_Hash
        (["parser",
         "version",
         "allowedTags"]);

   end MarkdownClientCapabilities_Scope;

   procedure Read_MarkdownClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkdownClientCapabilities) is
      use MarkdownClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MarkdownClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  parser
                  Value.parser.Clear;
                  Value.parser.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  version
                  Value.version.Clear;
                  Value.version.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  allowedTags
                  Read_Virtual_String_Vector (Handler, Value.allowedTags);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_MarkdownClientCapabilities;

   procedure Read_InlayHint_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHint_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_InlayHint_Vector (Handler, Value);
      end if;
   end Read_InlayHint_Vector_Or_Null;

   package InlayHint_Scope is
      package InlayHint_Map is new Minimal_Perfect_Hash
        (["position",
         "label",
         "kind",
         "textEdits",
         "tooltip",
         "paddingLeft",
         "paddingRight",
         "data"]);

   end InlayHint_Scope;

   procedure Read_InlayHint
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHint) is
      use InlayHint_Scope;
      procedure Read_Virtual_String_Or_InlayHintLabelPart_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .Virtual_String_Or_InlayHintLabelPart_Vector);

      procedure Read_InlayHintLabelPart_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.InlayHintLabelPart_Vector);

      procedure Read_Virtual_String_Or_InlayHintLabelPart_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .Virtual_String_Or_InlayHintLabelPart_Vector) is
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
               Read_InlayHintLabelPart_Vector
                 (Handler, Value.InlayHintLabelPart_Vector);
         end case;
      end Read_Virtual_String_Or_InlayHintLabelPart_Vector;

      procedure Read_InlayHintLabelPart_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.InlayHintLabelPart_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.InlayHintLabelPart_Vector renames Value;
            Value : LSP.Structures.InlayHintLabelPart;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_InlayHintLabelPart (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_InlayHintLabelPart_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlayHint_Map.Get_Index (Key) is
               when 1 =>  --  position
                  Read_Position (Handler, Value.position);
               when 2 =>  --  label
                  Read_Virtual_String_Or_InlayHintLabelPart_Vector
                    (Handler, Value.label);
               when 3 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InlayHintKind (Handler, Value.kind.Value);
               when 4 =>  --  textEdits
                  Read_TextEdit_Vector (Handler, Value.textEdits);
               when 5 =>  --  tooltip
                  Value.tooltip :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_MarkupContent
                    (Handler, Value.tooltip.Value);
               when 6 =>  --  paddingLeft
                  Value.paddingLeft       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.paddingLeft.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 7 =>  --  paddingRight
                  Value.paddingRight       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.paddingRight.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 8 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlayHint;

   package MonikerRegistrationOptions_Scope is
      package MonikerRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end MonikerRegistrationOptions_Scope;

   procedure Read_MonikerRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerRegistrationOptions) is
      use MonikerRegistrationOptions_Scope;
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
            case MonikerRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_MonikerRegistrationOptions;

   procedure Read_Pattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Pattern) is
   begin
      Value.Clear;
      Value.Append (Handler.String_Value);
      Handler.Read_Next;
   end Read_Pattern;

   procedure Read_DiagnosticTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticTag_Set) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DiagnosticTag_Set renames Value;
         Value : LSP.Enumerations.DiagnosticTag;
      begin
         Set := (others => False);
         while not Handler.Is_End_Array loop
            Read_DiagnosticTag (Handler, Value);
            Set (Value) := True;
         end loop;
      end;

      Handler.Read_Next;
   end Read_DiagnosticTag_Set;

   procedure Read_AlsSearchKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.AlsSearchKind) is
   begin
      Value :=
        LSP.Enumerations.AlsSearchKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_AlsSearchKind;

   procedure Read_SemanticTokensEdit_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensEdit_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.SemanticTokensEdit_Vector renames Value;
         Value : LSP.Structures.SemanticTokensEdit;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_SemanticTokensEdit (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_SemanticTokensEdit_Vector;

   procedure Read_FoldingRange_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRange_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.FoldingRange_Vector renames Value;
         Value : LSP.Structures.FoldingRange;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_FoldingRange (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_FoldingRange_Vector;

   package ExecuteCommandOptions_Scope is
      package ExecuteCommandOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "commands"]);

   end ExecuteCommandOptions_Scope;

   procedure Read_ExecuteCommandOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandOptions) is
      use ExecuteCommandOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ExecuteCommandOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  commands
                  Read_Virtual_String_Vector (Handler, Value.commands);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ExecuteCommandOptions;

   package MarkupKind_Map is new Minimal_Perfect_Hash
     (["plaintext",
      "markdown"]);

   procedure Read_MarkupKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.MarkupKind) is
   begin
      Value :=
        LSP.Enumerations.MarkupKind'Val
          (MarkupKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_MarkupKind;

   package CallHierarchyPrepareParams_Scope is
      package CallHierarchyPrepareParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken"]);

   end CallHierarchyPrepareParams_Scope;

   procedure Read_CallHierarchyPrepareParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyPrepareParams) is
      use CallHierarchyPrepareParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyPrepareParams_Map.Get_Index (Key) is
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
   end Read_CallHierarchyPrepareParams;

end LSP.Inputs.Part_14;
