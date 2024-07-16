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

package body LSP.Inputs.Part_15 is

   package CodeActionKind_Map is new Minimal_Perfect_Hash
     (["",
      "quickfix",
      "refactor",
      "refactor.extract",
      "refactor.inline",
      "refactor.rewrite",
      "source",
      "source.organizeImports",
      "source.fixAll"]);

   procedure Read_CodeActionKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CodeActionKind) is
   begin
      Value := (Handler.String_Value with null record);
      Handler.Read_Next;
   end Read_CodeActionKind;

   package ReferenceOptions_Scope is
      package ReferenceOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end ReferenceOptions_Scope;

   procedure Read_ReferenceOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceOptions) is
      use ReferenceOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ReferenceOptions_Map.Get_Index (Key) is
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
   end Read_ReferenceOptions;

   package CodeDescription_Scope is
      package CodeDescription_Map is new Minimal_Perfect_Hash (["href"]);

   end CodeDescription_Scope;

   procedure Read_CodeDescription
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeDescription) is
      use CodeDescription_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeDescription_Map.Get_Index (Key) is
               when 1 =>  --  href
                  Read_URI (Handler, Value.href);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeDescription;

   package CompletionClientCapabilities_Scope is
      package CompletionClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "completionItem",
         "completionItemKind",
         "insertTextMode",
         "contextSupport",
         "completionList"]);

      package completionItem_OfCompletionClientCapabilities_Scope is
         package completionItem_OfCompletionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["snippetSupport",
            "commitCharactersSupport",
            "documentationFormat",
            "deprecatedSupport",
            "preselectSupport",
            "tagSupport",
            "insertReplaceSupport",
            "resolveSupport",
            "insertTextModeSupport",
            "labelDetailsSupport"]);

      end completionItem_OfCompletionClientCapabilities_Scope;

      package completionList_OfCompletionClientCapabilities_Scope is
         package completionList_OfCompletionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["itemDefaults"]);

      end completionList_OfCompletionClientCapabilities_Scope;

      package tagSupport_OfcompletionItem_OfCompletionClientCapabilities_Scope
      is
         package tagSupport_OfcompletionItem_OfCompletionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["valueSet"]);

      end tagSupport_OfcompletionItem_OfCompletionClientCapabilities_Scope;

      package completionItemKind_OfCompletionClientCapabilities_Scope is
         package completionItemKind_OfCompletionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["valueSet"]);

      end completionItemKind_OfCompletionClientCapabilities_Scope;

      package insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities_Scope
      is
         package insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["valueSet"]);

      end insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities_Scope;

   end CompletionClientCapabilities_Scope;

   procedure Read_CompletionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionClientCapabilities) is
      use CompletionClientCapabilities_Scope;
      procedure Read_CompletionItemKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.CompletionItemKind_Set);

      procedure Read_completionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .completionItem_OfCompletionClientCapabilities);

      procedure Read_completionList_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .completionList_OfCompletionClientCapabilities);

      procedure Read_tagSupport_OfcompletionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .tagSupport_OfcompletionItem_OfCompletionClientCapabilities);

      procedure Read_InsertTextMode_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.InsertTextMode_Set);

      procedure Read_completionItemKind_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .completionItemKind_OfCompletionClientCapabilities);

      procedure Read_insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities);

      procedure Read_CompletionItemKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.CompletionItemKind_Set) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.CompletionItemKind_Set renames Value;
            Value : LSP.Enumerations.CompletionItemKind;
         begin
            Set := (others => False);
            while not Handler.Is_End_Array loop
               Read_CompletionItemKind (Handler, Value);
               Set (Value) := True;
            end loop;
         end;

         Handler.Read_Next;
      end Read_CompletionItemKind_Set;

      procedure Read_completionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .completionItem_OfCompletionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use completionItem_OfCompletionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case completionItem_OfCompletionClientCapabilities_Map.Get_Index
                 (Key) is
                  when 1 =>  --  snippetSupport
                     Value.snippetSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.snippetSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when 2 =>  --  commitCharactersSupport
                     Value.commitCharactersSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.commitCharactersSupport.Value :=
                       Handler.Boolean_Value;
                     Handler.Read_Next;
                  when 3 =>  --  documentationFormat
                     Read_MarkupKind_Vector
                       (Handler, Value.documentationFormat);
                  when 4 =>  --  deprecatedSupport
                     Value.deprecatedSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.deprecatedSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when 5 =>  --  preselectSupport
                     Value.preselectSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.preselectSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when 6 =>  --  tagSupport
                     Value.tagSupport :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_tagSupport_OfcompletionItem_OfCompletionClientCapabilities
                       (Handler, Value.tagSupport.Value);
                  when 7 =>  --  insertReplaceSupport
                     Value.insertReplaceSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.insertReplaceSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when 8 =>  --  resolveSupport
                     Value.resolveSupport :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_resolveSupport_OfWorkspaceSymbolClientCapabilities
                       (Handler, Value.resolveSupport.Value);
                  when 9 =>  --  insertTextModeSupport
                     Value.insertTextModeSupport :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities
                       (Handler, Value.insertTextModeSupport.Value);
                  when 10 =>  --  labelDetailsSupport
                     Value.labelDetailsSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.labelDetailsSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_completionItem_OfCompletionClientCapabilities;

      procedure Read_completionList_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .completionList_OfCompletionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use completionList_OfCompletionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case completionList_OfCompletionClientCapabilities_Map.Get_Index
                 (Key) is
                  when 1 =>  --  itemDefaults
                     Read_Virtual_String_Vector (Handler, Value.itemDefaults);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_completionList_OfCompletionClientCapabilities;

      procedure Read_tagSupport_OfcompletionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .tagSupport_OfcompletionItem_OfCompletionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 tagSupport_OfcompletionItem_OfCompletionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case tagSupport_OfcompletionItem_OfCompletionClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  valueSet
                     Read_CompletionItemTag_Set (Handler, Value.valueSet);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_tagSupport_OfcompletionItem_OfCompletionClientCapabilities;

      procedure Read_InsertTextMode_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.InsertTextMode_Set) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.InsertTextMode_Set renames Value;
            Value : LSP.Enumerations.InsertTextMode;
         begin
            Set := (others => False);
            while not Handler.Is_End_Array loop
               Read_InsertTextMode (Handler, Value);
               Set (Value) := True;
            end loop;
         end;

         Handler.Read_Next;
      end Read_InsertTextMode_Set;

      procedure Read_completionItemKind_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .completionItemKind_OfCompletionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use completionItemKind_OfCompletionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case completionItemKind_OfCompletionClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  valueSet
                     Read_CompletionItemKind_Set (Handler, Value.valueSet);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_completionItemKind_OfCompletionClientCapabilities;

      procedure Read_insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  valueSet
                     Read_InsertTextMode_Set (Handler, Value.valueSet);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  completionItem
                  Value.completionItem :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_completionItem_OfCompletionClientCapabilities
                    (Handler, Value.completionItem.Value);
               when 3 =>  --  completionItemKind
                  Value.completionItemKind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_completionItemKind_OfCompletionClientCapabilities
                    (Handler, Value.completionItemKind.Value);
               when 4 =>  --  insertTextMode
                  Value.insertTextMode :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InsertTextMode (Handler, Value.insertTextMode.Value);
               when 5 =>  --  contextSupport
                  Value.contextSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.contextSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  completionList
                  Value.completionList :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_completionList_OfCompletionClientCapabilities
                    (Handler, Value.completionList.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionClientCapabilities;

   package DocumentRangeFormattingParams_Scope is
      package DocumentRangeFormattingParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "textDocument",
         "range",
         "options"]);

   end DocumentRangeFormattingParams_Scope;

   procedure Read_DocumentRangeFormattingParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentRangeFormattingParams) is
      use DocumentRangeFormattingParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentRangeFormattingParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 3 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 4 =>  --  options
                  Read_FormattingOptions (Handler, Value.options);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentRangeFormattingParams;

   package WorkDoneProgressBegin_Scope is
      package WorkDoneProgressBegin_Map is new Minimal_Perfect_Hash
        (["kind",
         "title",
         "cancellable",
         "message",
         "percentage"]);

   end WorkDoneProgressBegin_Scope;

   procedure Read_WorkDoneProgressBegin
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressBegin) is
      use WorkDoneProgressBegin_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkDoneProgressBegin_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: begin
               when 2 =>  --  title
                  Value.title.Clear;
                  Value.title.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  cancellable
                  Value.cancellable       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.cancellable.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  percentage
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
   end Read_WorkDoneProgressBegin;

   procedure Read_Location_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Location_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Location_Vector renames Value;
         Value : LSP.Structures.Location;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_Location (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Location_Vector;

end LSP.Inputs.Part_15;
