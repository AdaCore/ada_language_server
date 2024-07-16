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

package body LSP.Inputs.Part_28 is

   package InlineValueParams_Scope is
      package InlineValueParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "textDocument",
         "range",
         "context"]);

   end InlineValueParams_Scope;

   procedure Read_InlineValueParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueParams) is
      use InlineValueParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 3 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 4 =>  --  context
                  Read_InlineValueContext (Handler, Value.context);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueParams;

   procedure Read_URI
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.URI) is
   begin
      Value.Clear;
      Value.Append (Handler.String_Value);
      Handler.Read_Next;
   end Read_URI;

   procedure Read_PrepareRenameResult_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PrepareRenameResult_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_PrepareRenameResult (Handler, Value.Value);
      end if;
   end Read_PrepareRenameResult_Or_Null;

   package NotebookDocumentSyncClientCapabilities_Scope is
      package NotebookDocumentSyncClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "executionSummarySupport"]);

   end NotebookDocumentSyncClientCapabilities_Scope;

   procedure Read_NotebookDocumentSyncClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentSyncClientCapabilities) is
      use NotebookDocumentSyncClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentSyncClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  executionSummarySupport
                  Value.executionSummarySupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.executionSummarySupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocumentSyncClientCapabilities;

   package DidChangeWatchedFilesParams_Scope is
      package DidChangeWatchedFilesParams_Map is new Minimal_Perfect_Hash
        (["changes"]);

   end DidChangeWatchedFilesParams_Scope;

   procedure Read_DidChangeWatchedFilesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeWatchedFilesParams) is
      use DidChangeWatchedFilesParams_Scope;
      procedure Read_FileEvent_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileEvent_Vector);

      procedure Read_FileEvent_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileEvent_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FileEvent_Vector renames Value;
            Value : LSP.Structures.FileEvent;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FileEvent (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FileEvent_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeWatchedFilesParams_Map.Get_Index (Key) is
               when 1 =>  --  changes
                  Read_FileEvent_Vector (Handler, Value.changes);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeWatchedFilesParams;

   procedure Read_MarkupKind_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkupKind_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.MarkupKind_Vector renames Value;
         Value : LSP.Enumerations.MarkupKind;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_MarkupKind (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_MarkupKind_Vector;

   package TypeHierarchyRegistrationOptions_Scope is
      package TypeHierarchyRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end TypeHierarchyRegistrationOptions_Scope;

   procedure Read_TypeHierarchyRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyRegistrationOptions) is
      use TypeHierarchyRegistrationOptions_Scope;
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
            case TypeHierarchyRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_TypeHierarchyRegistrationOptions;

   procedure Read_FoldingRange_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRange_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_FoldingRange_Vector (Handler, Value);
      end if;
   end Read_FoldingRange_Vector_Or_Null;

   package ExecutionSummary_Scope is
      package ExecutionSummary_Map is new Minimal_Perfect_Hash
        (["executionOrder",
         "success"]);

   end ExecutionSummary_Scope;

   procedure Read_ExecutionSummary
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecutionSummary) is
      use ExecutionSummary_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ExecutionSummary_Map.Get_Index (Key) is
               when 1 =>  --  executionOrder
                  Value.executionOrder :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  success
                  Value.success       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.success.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ExecutionSummary;

   package VersionedNotebookDocumentIdentifier_Scope is
      package VersionedNotebookDocumentIdentifier_Map is new Minimal_Perfect_Hash
        (["version",
         "uri"]);

   end VersionedNotebookDocumentIdentifier_Scope;

   procedure Read_VersionedNotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.VersionedNotebookDocumentIdentifier) is
      use VersionedNotebookDocumentIdentifier_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case VersionedNotebookDocumentIdentifier_Map.Get_Index (Key) is
               when 1 =>  --  version
                  Value.version :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  uri
                  Read_URI (Handler, Value.uri);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_VersionedNotebookDocumentIdentifier;

   package SemanticTokensEdit_Scope is
      package SemanticTokensEdit_Map is new Minimal_Perfect_Hash
        (["start",
         "deleteCount",
         "data"]);

   end SemanticTokensEdit_Scope;

   procedure Read_SemanticTokensEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensEdit) is
      use SemanticTokensEdit_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensEdit_Map.Get_Index (Key) is
               when 1 =>  --  start
                  Value.start := Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  deleteCount
                  Value.deleteCount :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  data
                  Read_Natural_Vector (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensEdit;

   package FoldingRange_Scope is
      package FoldingRange_Map is new Minimal_Perfect_Hash
        (["startLine",
         "startCharacter",
         "endLine",
         "endCharacter",
         "kind",
         "collapsedText"]);

   end FoldingRange_Scope;

   procedure Read_FoldingRange
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRange) is
      use FoldingRange_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FoldingRange_Map.Get_Index (Key) is
               when 1 =>  --  startLine
                  Value.startLine :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  startCharacter
                  Value.startCharacter       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.startCharacter.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  endLine
                  Value.endLine :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 4 =>  --  endCharacter
                  Value.endCharacter       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.endCharacter.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 5 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FoldingRangeKind (Handler, Value.kind.Value);
               when 6 =>  --  collapsedText
                  Value.collapsedText.Clear;
                  Value.collapsedText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FoldingRange;

   package CallHierarchyIncomingCallsParams_Scope is
      package CallHierarchyIncomingCallsParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "item"]);

   end CallHierarchyIncomingCallsParams_Scope;

   procedure Read_CallHierarchyIncomingCallsParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyIncomingCallsParams) is
      use CallHierarchyIncomingCallsParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyIncomingCallsParams_Map.Get_Index (Key) is
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
               when 3 =>  --  item
                  Read_CallHierarchyItem (Handler, Value.item);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyIncomingCallsParams;

   package SelectionRangeRegistrationOptions_Scope is
      package SelectionRangeRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "documentSelector",
         "id"]);

   end SelectionRangeRegistrationOptions_Scope;

   procedure Read_SelectionRangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeRegistrationOptions) is
      use SelectionRangeRegistrationOptions_Scope;
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
            case SelectionRangeRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
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
   end Read_SelectionRangeRegistrationOptions;

   package WorkspaceDiagnosticParams_Scope is
      package WorkspaceDiagnosticParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "identifier",
         "previousResultIds"]);

   end WorkspaceDiagnosticParams_Scope;

   procedure Read_WorkspaceDiagnosticParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDiagnosticParams) is
      use WorkspaceDiagnosticParams_Scope;
      procedure Read_PreviousResultId_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PreviousResultId_Vector);

      procedure Read_PreviousResultId_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PreviousResultId_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.PreviousResultId_Vector renames Value;
            Value : LSP.Structures.PreviousResultId;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_PreviousResultId (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_PreviousResultId_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceDiagnosticParams_Map.Get_Index (Key) is
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
               when 3 =>  --  identifier
                  Value.identifier.Clear;
                  Value.identifier.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 4 =>  --  previousResultIds
                  Read_PreviousResultId_Vector
                    (Handler, Value.previousResultIds);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceDiagnosticParams;

   procedure Read_InlayHint_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHint_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.InlayHint_Vector renames Value;
         Value : LSP.Structures.InlayHint;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_InlayHint (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_InlayHint_Vector;

   package DocumentColorParams_Scope is
      package DocumentColorParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument"]);

   end DocumentColorParams_Scope;

   procedure Read_DocumentColorParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorParams) is
      use DocumentColorParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentColorParams_Map.Get_Index (Key) is
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
   end Read_DocumentColorParams;

   procedure Read_CompletionItemTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItemTag_Set) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CompletionItemTag_Set renames Value;
         Value : LSP.Enumerations.CompletionItemTag;
      begin
         Set := (others => False);
         while not Handler.Is_End_Array loop
            Read_CompletionItemTag (Handler, Value);
            Set (Value) := True;
         end loop;
      end;

      Handler.Read_Next;
   end Read_CompletionItemTag_Set;

   package DocumentLinkRegistrationOptions_Scope is
      package DocumentLinkRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "resolveProvider"]);

   end DocumentLinkRegistrationOptions_Scope;

   procedure Read_DocumentLinkRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkRegistrationOptions) is
      use DocumentLinkRegistrationOptions_Scope;
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
            case DocumentLinkRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
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
   end Read_DocumentLinkRegistrationOptions;

   package UniquenessLevel_Map is new Minimal_Perfect_Hash
     (["document",
      "project",
      "group",
      "scheme",
      "global"]);

   procedure Read_UniquenessLevel
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.UniquenessLevel) is
   begin
      Value :=
        LSP.Enumerations.UniquenessLevel'Val
          (UniquenessLevel_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_UniquenessLevel;

   package LinkedEditingRangeParams_Scope is
      package LinkedEditingRangeParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken"]);

   end LinkedEditingRangeParams_Scope;

   procedure Read_LinkedEditingRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeParams) is
      use LinkedEditingRangeParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LinkedEditingRangeParams_Map.Get_Index (Key) is
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
   end Read_LinkedEditingRangeParams;

   package ImplementationParams_Scope is
      package ImplementationParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken",
         "alsDisplayMethodAncestryOnNavigation"]);

   end ImplementationParams_Scope;

   procedure Read_ImplementationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationParams) is
      use ImplementationParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ImplementationParams_Map.Get_Index (Key) is
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
               when 5 =>  --  alsDisplayMethodAncestryOnNavigation
                  Value.alsDisplayMethodAncestryOnNavigation :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_AlsDisplayMethodAncestryOnNavigationPolicy
                    (Handler,
                     Value.alsDisplayMethodAncestryOnNavigation.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ImplementationParams;

end LSP.Inputs.Part_28;
