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

package body LSP.Inputs.Part_3 is

   procedure Read_CallHierarchyOutgoingCall_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOutgoingCall_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CallHierarchyOutgoingCall_Vector renames Value;
         Value : LSP.Structures.CallHierarchyOutgoingCall;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_CallHierarchyOutgoingCall (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_CallHierarchyOutgoingCall_Vector;

   procedure Read_ColorInformation_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorInformation_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.ColorInformation_Vector renames Value;
         Value : LSP.Structures.ColorInformation;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_ColorInformation (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_ColorInformation_Vector;

   package tagSupport_OfWorkspaceSymbolClientCapabilities_Scope is
      package tagSupport_OfWorkspaceSymbolClientCapabilities_Map is new Minimal_Perfect_Hash
        (["valueSet"]);

   end tagSupport_OfWorkspaceSymbolClientCapabilities_Scope;

   procedure Read_tagSupport_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .tagSupport_OfWorkspaceSymbolClientCapabilities) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            use tagSupport_OfWorkspaceSymbolClientCapabilities_Scope;
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case tagSupport_OfWorkspaceSymbolClientCapabilities_Map.Get_Index
              (Key) is
               when 1 =>  --  valueSet
                  Read_SymbolTag_Set (Handler, Value.valueSet);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_tagSupport_OfWorkspaceSymbolClientCapabilities;

   package DiagnosticOptions_Scope is
      package DiagnosticOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "identifier",
         "interFileDependencies",
         "workspaceDiagnostics"]);

   end DiagnosticOptions_Scope;

   procedure Read_DiagnosticOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticOptions) is
      use DiagnosticOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DiagnosticOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  identifier
                  Value.identifier.Clear;
                  Value.identifier.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  interFileDependencies
                  Value.interFileDependencies := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  workspaceDiagnostics
                  Value.workspaceDiagnostics := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DiagnosticOptions;

   package DidOpenNotebookDocumentParams_Scope is
      package DidOpenNotebookDocumentParams_Map is new Minimal_Perfect_Hash
        (["notebookDocument",
         "cellTextDocuments"]);

   end DidOpenNotebookDocumentParams_Scope;

   procedure Read_DidOpenNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidOpenNotebookDocumentParams) is
      use DidOpenNotebookDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidOpenNotebookDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  notebookDocument
                  Read_NotebookDocument (Handler, Value.notebookDocument);
               when 2 =>  --  cellTextDocuments
                  Read_TextDocumentItem_Vector
                    (Handler, Value.cellTextDocuments);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidOpenNotebookDocumentParams;

   package ResourceOperationKind_Map is new Minimal_Perfect_Hash
     (["create",
      "rename",
      "delete"]);

   procedure Read_ResourceOperationKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.ResourceOperationKind) is
   begin
      Value :=
        LSP.Enumerations.ResourceOperationKind'Val
          (ResourceOperationKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_ResourceOperationKind;

   procedure Read_Natural_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Natural_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Natural_Vector renames Value;
         Value : Natural;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Value := Integer (Handler.Number_Value.Integer_Value);
            Handler.Read_Next;
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Natural_Vector;

   package ReferenceParams_Scope is
      package ReferenceParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken",
         "context"]);

   end ReferenceParams_Scope;

   procedure Read_ReferenceParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceParams) is
      use ReferenceParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ReferenceParams_Map.Get_Index (Key) is
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
               when 5 =>  --  context
                  Read_ReferenceContext (Handler, Value.context);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ReferenceParams;

   package FailureHandlingKind_Map is new Minimal_Perfect_Hash
     (["abort",
      "transactional",
      "textOnlyTransactional",
      "undo"]);

   procedure Read_FailureHandlingKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FailureHandlingKind) is
   begin
      Value :=
        LSP.Enumerations.FailureHandlingKind'Val
          (FailureHandlingKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_FailureHandlingKind;

   package ExecuteCommandClientCapabilities_Scope is
      package ExecuteCommandClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end ExecuteCommandClientCapabilities_Scope;

   procedure Read_ExecuteCommandClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandClientCapabilities) is
      use ExecuteCommandClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ExecuteCommandClientCapabilities_Map.Get_Index (Key) is
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
   end Read_ExecuteCommandClientCapabilities;

   package HoverRegistrationOptions_Scope is
      package HoverRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end HoverRegistrationOptions_Scope;

   procedure Read_HoverRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverRegistrationOptions) is
      use HoverRegistrationOptions_Scope;
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
            case HoverRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_HoverRegistrationOptions;

   package DocumentRangeFormattingOptions_Scope is
      package DocumentRangeFormattingOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end DocumentRangeFormattingOptions_Scope;

   procedure Read_DocumentRangeFormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentRangeFormattingOptions) is
      use DocumentRangeFormattingOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentRangeFormattingOptions_Map.Get_Index (Key) is
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
   end Read_DocumentRangeFormattingOptions;

   package Command_Scope is
      package Command_Map is new Minimal_Perfect_Hash
        (["title",
         "command",
         "arguments"]);

   end Command_Scope;

   procedure Read_Command
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command) is
      use Command_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Command_Map.Get_Index (Key) is
               when 1 =>  --  title
                  Value.title.Clear;
                  Value.title.Append (Handler.String_Value);
                  Handler.Read_Next;
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
   end Read_Command;

   package Diagnostic_Scope is
      package Diagnostic_Map is new Minimal_Perfect_Hash
        (["range",
         "severity",
         "code",
         "codeDescription",
         "source",
         "message",
         "tags",
         "relatedInformation",
         "data"]);

   end Diagnostic_Scope;

   procedure Read_Diagnostic
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Diagnostic) is
      use Diagnostic_Scope;
      procedure Read_DiagnosticRelatedInformation_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DiagnosticRelatedInformation_Vector);

      procedure Read_DiagnosticRelatedInformation_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DiagnosticRelatedInformation_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   :
              LSP.Structures.DiagnosticRelatedInformation_Vector renames Value;
            Value : LSP.Structures.DiagnosticRelatedInformation;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_DiagnosticRelatedInformation (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_DiagnosticRelatedInformation_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Diagnostic_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  severity
                  Value.severity :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DiagnosticSeverity (Handler, Value.severity.Value);
               when 3 =>  --  code
                  Value.code :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Integer_Or_Virtual_String (Handler, Value.code.Value);
               when 4 =>  --  codeDescription
                  Value.codeDescription :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeDescription (Handler, Value.codeDescription.Value);
               when 5 =>  --  source
                  Value.source.Clear;
                  Value.source.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 6 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 7 =>  --  tags
                  Read_DiagnosticTag_Set (Handler, Value.tags);
               when 8 =>  --  relatedInformation
                  Read_DiagnosticRelatedInformation_Vector
                    (Handler, Value.relatedInformation);
               when 9 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Diagnostic;

end LSP.Inputs.Part_3;
