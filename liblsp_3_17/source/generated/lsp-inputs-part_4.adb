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

package body LSP.Inputs.Part_4 is

   package Declaration_Result_Scope is
      package Declaration_Result_Map is new Minimal_Perfect_Hash
        (["uri",
         "range",
         "alsKind"]);

   end Declaration_Result_Scope;

   procedure Read_Declaration_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Declaration_Result) is
      use Declaration_Result_Scope;
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
                    Declaration_Result_Map.Get_Index (Key);
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
               Read_Declaration (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_DeclarationLink_Vector (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               null;  --  #null_value
               Handler.Read_Next;
         end case;
      end;
   end Read_Declaration_Result;

   package RenameFile_Scope is
      package RenameFile_Map is new Minimal_Perfect_Hash
        (["kind",
         "annotationId",
         "oldUri",
         "newUri",
         "options"]);

   end RenameFile_Scope;

   procedure Read_RenameFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameFile) is
      use RenameFile_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameFile_Map.Get_Index (Key) is
               when 2 =>  --  annotationId
                  Value.annotationId :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ChangeAnnotationIdentifier
                    (Handler, Value.annotationId.Value);
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: rename
               when 3 =>  --  oldUri
                  Value.oldUri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 4 =>  --  newUri
                  Value.newUri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 5 =>  --  options
                  Value.options :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_RenameFileOptions (Handler, Value.options.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameFile;

   package InlineValueVariableLookup_Scope is
      package InlineValueVariableLookup_Map is new Minimal_Perfect_Hash
        (["range",
         "variableName",
         "caseSensitiveLookup"]);

   end InlineValueVariableLookup_Scope;

   procedure Read_InlineValueVariableLookup
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueVariableLookup) is
      use InlineValueVariableLookup_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueVariableLookup_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  variableName
                  Value.variableName.Clear;
                  Value.variableName.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  caseSensitiveLookup
                  Value.caseSensitiveLookup := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueVariableLookup;

   package WorkDoneProgressCancelParams_Scope is
      package WorkDoneProgressCancelParams_Map is new Minimal_Perfect_Hash
        (["token"]);

   end WorkDoneProgressCancelParams_Scope;

   procedure Read_WorkDoneProgressCancelParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressCancelParams) is
      use WorkDoneProgressCancelParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkDoneProgressCancelParams_Map.Get_Index (Key) is
               when 1 =>  --  token
                  Read_ProgressToken (Handler, Value.token);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkDoneProgressCancelParams;

   procedure Read_ProgressToken
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ProgressToken) is
   begin
      if Handler.Is_Number_Value then
         Value :=
           (Is_Integer => True,
            others     => <>);
      else
         Value :=
           (Is_Integer => False,
            others     => <>);
      end if;

      case Value.Is_Integer is
         when True =>
            Value.Integer := Integer (Handler.Number_Value.Integer_Value);
            Handler.Read_Next;
         when False =>
            Value.Virtual_String.Clear;
            Value.Virtual_String.Append (Handler.String_Value);
            Handler.Read_Next;
      end case;
   end Read_ProgressToken;

   package DidSaveTextDocumentParams_Scope is
      package DidSaveTextDocumentParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "text"]);

   end DidSaveTextDocumentParams_Scope;

   procedure Read_DidSaveTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidSaveTextDocumentParams) is
      use DidSaveTextDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidSaveTextDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
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
   end Read_DidSaveTextDocumentParams;

   procedure Read_CodeActionKind_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionKind_Set) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CodeActionKind_Set renames Value;
         Value : LSP.Enumerations.CodeActionKind;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_CodeActionKind (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_CodeActionKind_Set;

   procedure Read_CallHierarchyItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CallHierarchyItem_Vector renames Value;
         Value : LSP.Structures.CallHierarchyItem;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_CallHierarchyItem (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_CallHierarchyItem_Vector;

   package NotebookDocument_Scope is
      package NotebookDocument_Map is new Minimal_Perfect_Hash
        (["uri",
         "notebookType",
         "version",
         "metadata",
         "cells"]);

   end NotebookDocument_Scope;

   procedure Read_NotebookDocument
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocument) is
      use NotebookDocument_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocument_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Read_URI (Handler, Value.uri);
               when 2 =>  --  notebookType
                  Value.notebookType.Clear;
                  Value.notebookType.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  version
                  Value.version :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 4 =>  --  metadata
                  Value.metadata :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_LSPObject (Handler, Value.metadata.Value);
               when 5 =>  --  cells
                  Read_NotebookCell_Vector (Handler, Value.cells);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocument;

   package CodeLensClientCapabilities_Scope is
      package CodeLensClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end CodeLensClientCapabilities_Scope;

   procedure Read_CodeLensClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensClientCapabilities) is
      use CodeLensClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeLensClientCapabilities_Map.Get_Index (Key) is
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
   end Read_CodeLensClientCapabilities;

   package CompletionList_Scope is
      package CompletionList_Map is new Minimal_Perfect_Hash
        (["isIncomplete",
         "itemDefaults",
         "items"]);

      package itemDefaults_OfCompletionList_Scope is
         package itemDefaults_OfCompletionList_Map is new Minimal_Perfect_Hash
           (["commitCharacters",
            "editRange",
            "insertTextFormat",
            "insertTextMode",
            "data"]);

      end itemDefaults_OfCompletionList_Scope;

      package Range_Or_Something_Scope is
         package Range_Or_Something_Map is new Minimal_Perfect_Hash
           (["start",
            "end",
            "insert",
            "replace"]);

      end Range_Or_Something_Scope;

   end CompletionList_Scope;

   procedure Read_CompletionList
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionList) is
      use CompletionList_Scope;
      procedure Read_itemDefaults_OfCompletionList
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.itemDefaults_OfCompletionList);

      procedure Read_Range_Or_Something
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Range_Or_Something);

      procedure Read_itemDefaults_OfCompletionList
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.itemDefaults_OfCompletionList) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use itemDefaults_OfCompletionList_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case itemDefaults_OfCompletionList_Map.Get_Index (Key) is
                  when 1 =>  --  commitCharacters
                     Read_Virtual_String_Vector
                       (Handler, Value.commitCharacters);
                  when 2 =>  --  editRange
                     Value.editRange :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_Range_Or_Something (Handler, Value.editRange.Value);
                  when 3 =>  --  insertTextFormat
                     Value.insertTextFormat :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_InsertTextFormat
                       (Handler, Value.insertTextFormat.Value);
                  when 4 =>  --  insertTextMode
                     Value.insertTextMode :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_InsertTextMode (Handler, Value.insertTextMode.Value);
                  when 5 =>  --  data
                     Read_LSPAny (Handler, Value.data);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_itemDefaults_OfCompletionList;

      procedure Read_Range_Or_Something
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Range_Or_Something) is
         use Range_Or_Something_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       Range_Or_Something_Map.Get_Index (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  start
                           Value :=
                             (Is_A_Range => True,
                              others     => <>);
                           exit;
                        when 2 =>  --  end
                           Value :=
                             (Is_A_Range => True,
                              others     => <>);
                           exit;
                        when 3 =>  --  insert
                           Value :=
                             (Is_A_Range => False,
                              others     => <>);
                           exit;
                        when 4 =>  --  replace
                           Value :=
                             (Is_A_Range => False,
                              others     => <>);
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

            case Value.Is_A_Range is
               when True =>
                  Read_A_Range (Handler, Value.A_Range);
               when False =>
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while Handler.Is_Key_Name loop
                     declare
                        Key : constant VSS.Strings.Virtual_String :=
                          Handler.Key_Name;
                     begin
                        Handler.Read_Next;
                        case Range_Or_Something_Map.Get_Index (Key) is
                           when 3 =>  --  insert
                              Read_A_Range (Handler, Value.insert);
                           when 4 =>  --  replace
                              Read_A_Range (Handler, Value.replace);
                           when others =>
                              Handler.Skip_Current_Value;
                        end case;
                     end;
                  end loop;

                  Handler.Read_Next;
            end case;
         end;
      end Read_Range_Or_Something;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionList_Map.Get_Index (Key) is
               when 1 =>  --  isIncomplete
                  Value.isIncomplete := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  itemDefaults
                  Value.itemDefaults :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_itemDefaults_OfCompletionList
                    (Handler, Value.itemDefaults.Value);
               when 3 =>  --  items
                  Read_CompletionItem_Vector (Handler, Value.items);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionList;

   package DiagnosticRegistrationOptions_Scope is
      package DiagnosticRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "identifier",
         "interFileDependencies",
         "workspaceDiagnostics",
         "id"]);

   end DiagnosticRegistrationOptions_Scope;

   procedure Read_DiagnosticRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticRegistrationOptions) is
      use DiagnosticRegistrationOptions_Scope;
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
            case DiagnosticRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  identifier
                  Value.identifier.Clear;
                  Value.identifier.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 4 =>  --  interFileDependencies
                  Value.interFileDependencies := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  workspaceDiagnostics
                  Value.workspaceDiagnostics := Handler.Boolean_Value;
                  Handler.Read_Next;
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
   end Read_DiagnosticRegistrationOptions;

   package UnregistrationParams_Scope is
      package UnregistrationParams_Map is new Minimal_Perfect_Hash
        (["unregisterations"]);

   end UnregistrationParams_Scope;

   procedure Read_UnregistrationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.UnregistrationParams) is
      use UnregistrationParams_Scope;
      procedure Read_Unregistration_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Unregistration_Vector);

      procedure Read_Unregistration_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Unregistration_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.Unregistration_Vector renames Value;
            Value : LSP.Structures.Unregistration;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_Unregistration (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_Unregistration_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case UnregistrationParams_Map.Get_Index (Key) is
               when 1 =>  --  unregisterations
                  Read_Unregistration_Vector (Handler, Value.unregisterations);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_UnregistrationParams;

   package FoldingRangeKind_Map is new Minimal_Perfect_Hash
     (["comment",
      "imports",
      "region"]);

   procedure Read_FoldingRangeKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FoldingRangeKind) is
   begin
      Value := (Handler.String_Value with null record);
      Handler.Read_Next;
   end Read_FoldingRangeKind;

   procedure Read_DeclarationLink_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationLink_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DeclarationLink_Vector renames Value;
         Value : LSP.Structures.DeclarationLink;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_DeclarationLink (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_DeclarationLink_Vector;

   package WorkspaceDiagnosticReportPartialResult_Scope is
      package WorkspaceDiagnosticReportPartialResult_Map is new Minimal_Perfect_Hash
        (["items"]);

   end WorkspaceDiagnosticReportPartialResult_Scope;

   procedure Read_WorkspaceDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDiagnosticReportPartialResult) is
      use WorkspaceDiagnosticReportPartialResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceDiagnosticReportPartialResult_Map.Get_Index (Key) is
               when 1 =>  --  items
                  Read_WorkspaceDocumentDiagnosticReport_Vector
                    (Handler, Value.items);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceDiagnosticReportPartialResult;

   package FileRename_Scope is
      package FileRename_Map is new Minimal_Perfect_Hash
        (["oldUri",
         "newUri"]);

   end FileRename_Scope;

   procedure Read_FileRename
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileRename) is
      use FileRename_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileRename_Map.Get_Index (Key) is
               when 1 =>  --  oldUri
                  Value.oldUri.Clear;
                  Value.oldUri.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  newUri
                  Value.newUri.Clear;
                  Value.newUri.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileRename;

   package HoverOptions_Scope is
      package HoverOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end HoverOptions_Scope;

   procedure Read_HoverOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverOptions) is
      use HoverOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case HoverOptions_Map.Get_Index (Key) is
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
   end Read_HoverOptions;

   package DocumentRangeFormattingRegistrationOptions_Scope is
      package DocumentRangeFormattingRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end DocumentRangeFormattingRegistrationOptions_Scope;

   procedure Read_DocumentRangeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DocumentRangeFormattingRegistrationOptions) is
      use DocumentRangeFormattingRegistrationOptions_Scope;
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
            case DocumentRangeFormattingRegistrationOptions_Map.Get_Index
              (Key) is
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
   end Read_DocumentRangeFormattingRegistrationOptions;

   package ShowMessageRequestClientCapabilities_Scope is
      package ShowMessageRequestClientCapabilities_Map is new Minimal_Perfect_Hash
        (["messageActionItem"]);

      package messageActionItem_OfShowMessageRequestClientCapabilities_Scope is
         package messageActionItem_OfShowMessageRequestClientCapabilities_Map is new Minimal_Perfect_Hash
           (["additionalPropertiesSupport"]);

      end messageActionItem_OfShowMessageRequestClientCapabilities_Scope;

   end ShowMessageRequestClientCapabilities_Scope;

   procedure Read_ShowMessageRequestClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowMessageRequestClientCapabilities) is
      use ShowMessageRequestClientCapabilities_Scope;
      procedure Read_messageActionItem_OfShowMessageRequestClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .messageActionItem_OfShowMessageRequestClientCapabilities);

      procedure Read_messageActionItem_OfShowMessageRequestClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .messageActionItem_OfShowMessageRequestClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 messageActionItem_OfShowMessageRequestClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case messageActionItem_OfShowMessageRequestClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  additionalPropertiesSupport
                     Value.additionalPropertiesSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.additionalPropertiesSupport.Value :=
                       Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_messageActionItem_OfShowMessageRequestClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ShowMessageRequestClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  messageActionItem
                  Value.messageActionItem :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_messageActionItem_OfShowMessageRequestClientCapabilities
                    (Handler, Value.messageActionItem.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ShowMessageRequestClientCapabilities;

end LSP.Inputs.Part_4;
