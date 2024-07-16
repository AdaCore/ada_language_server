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

package body LSP.Inputs.Part_8 is

   package FileOperationRegistrationOptions_Scope is
      package FileOperationRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["filters"]);

   end FileOperationRegistrationOptions_Scope;

   procedure Read_FileOperationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationRegistrationOptions) is
      use FileOperationRegistrationOptions_Scope;
      procedure Read_FileOperationFilter_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileOperationFilter_Vector);

      procedure Read_FileOperationFilter_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileOperationFilter_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FileOperationFilter_Vector renames Value;
            Value : LSP.Structures.FileOperationFilter;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FileOperationFilter (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FileOperationFilter_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileOperationRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  filters
                  Read_FileOperationFilter_Vector (Handler, Value.filters);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileOperationRegistrationOptions;

   package CreateFile_Scope is
      package CreateFile_Map is new Minimal_Perfect_Hash
        (["kind",
         "annotationId",
         "uri",
         "options"]);

   end CreateFile_Scope;

   procedure Read_CreateFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFile) is
      use CreateFile_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CreateFile_Map.Get_Index (Key) is
               when 2 =>  --  annotationId
                  Value.annotationId :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ChangeAnnotationIdentifier
                    (Handler, Value.annotationId.Value);
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: create
               when 3 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 4 =>  --  options
                  Value.options :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CreateFileOptions (Handler, Value.options.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CreateFile;

   package SemanticTokens_Scope is
      package SemanticTokens_Map is new Minimal_Perfect_Hash
        (["resultId",
         "data"]);

   end SemanticTokens_Scope;

   procedure Read_SemanticTokens
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokens) is
      use SemanticTokens_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokens_Map.Get_Index (Key) is
               when 1 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  data
                  Read_Natural_Vector (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokens;

   package WorkDoneProgressReport_Scope is
      package WorkDoneProgressReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "cancellable",
         "message",
         "percentage"]);

   end WorkDoneProgressReport_Scope;

   procedure Read_WorkDoneProgressReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressReport) is
      use WorkDoneProgressReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkDoneProgressReport_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: report
               when 2 =>  --  cancellable
                  Value.cancellable       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.cancellable.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 4 =>  --  percentage
                  Value.percentage       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.percentage.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkDoneProgressReport;

   procedure Read_Boolean_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Boolean_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Boolean_Vector renames Value;
         Value : Standard.Boolean;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Value := Handler.Boolean_Value;
            Handler.Read_Next;
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Boolean_Vector;

   package WorkspaceSymbolRegistrationOptions_Scope is
      package WorkspaceSymbolRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "resolveProvider"]);

   end WorkspaceSymbolRegistrationOptions_Scope;

   procedure Read_WorkspaceSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolRegistrationOptions) is
      use WorkspaceSymbolRegistrationOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceSymbolRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_WorkspaceSymbolRegistrationOptions;

   package PositionEncodingKind_Map is new Minimal_Perfect_Hash
     (["utf-8",
      "utf-16",
      "utf-32"]);

   procedure Read_PositionEncodingKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.PositionEncodingKind) is
   begin
      Value := (Handler.String_Value with null record);
      Handler.Read_Next;
   end Read_PositionEncodingKind;

   package Completion_Result_Scope is
      package Completion_Result_Map is new Minimal_Perfect_Hash
        (["isIncomplete",
         "itemDefaults",
         "items"]);

   end Completion_Result_Scope;

   procedure Read_Completion_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Completion_Result) is
      use Completion_Result_Scope;
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
              (Kind   => LSP.Structures.Variant_1,
               others => <>);
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Completion_Result_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  isIncomplete
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 2 =>  --  itemDefaults
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 3 =>  --  items
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
               Read_CompletionItem_Vector (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_CompletionList (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               null;  --  #null_value
               Handler.Read_Next;
         end case;
      end;
   end Read_Completion_Result;

   package FileCreate_Scope is
      package FileCreate_Map is new Minimal_Perfect_Hash (["uri"]);

   end FileCreate_Scope;

   procedure Read_FileCreate
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileCreate) is
      use FileCreate_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileCreate_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri.Clear;
                  Value.uri.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileCreate;

   package InlineValueOptions_Scope is
      package InlineValueOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end InlineValueOptions_Scope;

   procedure Read_InlineValueOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueOptions) is
      use InlineValueOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueOptions_Map.Get_Index (Key) is
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
   end Read_InlineValueOptions;

   package ImplementationOptions_Scope is
      package ImplementationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end ImplementationOptions_Scope;

   procedure Read_ImplementationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationOptions) is
      use ImplementationOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ImplementationOptions_Map.Get_Index (Key) is
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
   end Read_ImplementationOptions;

   procedure Read_ChangeAnnotationIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ChangeAnnotationIdentifier) is
   begin
      Value.Clear;
      Value.Append (Handler.String_Value);
      Handler.Read_Next;
   end Read_ChangeAnnotationIdentifier;

   package InlayHintWorkspaceClientCapabilities_Scope is
      package InlayHintWorkspaceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["refreshSupport"]);

   end InlayHintWorkspaceClientCapabilities_Scope;

   procedure Read_InlayHintWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintWorkspaceClientCapabilities) is
      use InlayHintWorkspaceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlayHintWorkspaceClientCapabilities_Map.Get_Index (Key) is
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
   end Read_InlayHintWorkspaceClientCapabilities;

   package DocumentColorOptions_Scope is
      package DocumentColorOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end DocumentColorOptions_Scope;

   procedure Read_DocumentColorOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorOptions) is
      use DocumentColorOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentColorOptions_Map.Get_Index (Key) is
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
   end Read_DocumentColorOptions;

   package TypeHierarchyPrepareParams_Scope is
      package TypeHierarchyPrepareParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken"]);

   end TypeHierarchyPrepareParams_Scope;

   procedure Read_TypeHierarchyPrepareParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyPrepareParams) is
      use TypeHierarchyPrepareParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchyPrepareParams_Map.Get_Index (Key) is
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
   end Read_TypeHierarchyPrepareParams;

   package TextDocumentFilter_Scope is
      package TextDocumentFilter_Map is new Minimal_Perfect_Hash
        (["language",
         "scheme",
         "pattern"]);

   end TextDocumentFilter_Scope;

   procedure Read_TextDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentFilter) is
      use TextDocumentFilter_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentFilter_Map.Get_Index (Key) is
               when 1 =>  --  language
                  Value.language.Clear;
                  Value.language.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  scheme
                  Value.scheme.Clear;
                  Value.scheme.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  pattern
                  Value.pattern.Clear;
                  Value.pattern.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentFilter;

   package LinkedEditingRangeOptions_Scope is
      package LinkedEditingRangeOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end LinkedEditingRangeOptions_Scope;

   procedure Read_LinkedEditingRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeOptions) is
      use LinkedEditingRangeOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LinkedEditingRangeOptions_Map.Get_Index (Key) is
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
   end Read_LinkedEditingRangeOptions;

end LSP.Inputs.Part_8;
