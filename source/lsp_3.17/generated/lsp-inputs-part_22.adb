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

package body LSP.Inputs.Part_22 is

   procedure Read_Declaration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Declaration) is
   begin
      declare
         Set   : LSP.Structures.Declaration renames Value;
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

   end Read_Declaration;

   package RelatedUnchangedDocumentDiagnosticReport_Scope is
      package RelatedUnchangedDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "resultId",
         "relatedDocuments"]);

   end RelatedUnchangedDocumentDiagnosticReport_Scope;

   procedure Read_RelatedUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RelatedUnchangedDocumentDiagnosticReport) is
      use RelatedUnchangedDocumentDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RelatedUnchangedDocumentDiagnosticReport_Map.Get_Index
              (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: unchanged
               when 2 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  relatedDocuments
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
   end Read_RelatedUnchangedDocumentDiagnosticReport;

   package CallHierarchyItem_Scope is
      package CallHierarchyItem_Map is new Minimal_Perfect_Hash
        (["name",
         "kind",
         "tags",
         "detail",
         "uri",
         "range",
         "selectionRange",
         "data"]);

   end CallHierarchyItem_Scope;

   procedure Read_CallHierarchyItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem) is
      use CallHierarchyItem_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyItem_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  kind
                  Read_SymbolKind (Handler, Value.kind);
               when 3 =>  --  tags
                  Read_SymbolTag_Set (Handler, Value.tags);
               when 4 =>  --  detail
                  Value.detail.Clear;
                  Value.detail.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 6 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 7 =>  --  selectionRange
                  Read_A_Range (Handler, Value.selectionRange);
               when 8 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyItem;

   procedure Read_InlayHintKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InlayHintKind) is
   begin
      Value :=
        LSP.Enumerations.InlayHintKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_InlayHintKind;

   package RenameOptions_Scope is
      package RenameOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "prepareProvider"]);

   end RenameOptions_Scope;

   procedure Read_RenameOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameOptions) is
      use RenameOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  prepareProvider
                  Value.prepareProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.prepareProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameOptions;

   package DidChangeConfigurationClientCapabilities_Scope is
      package DidChangeConfigurationClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end DidChangeConfigurationClientCapabilities_Scope;

   procedure Read_DidChangeConfigurationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeConfigurationClientCapabilities) is
      use DidChangeConfigurationClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeConfigurationClientCapabilities_Map.Get_Index
              (Key) is
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
   end Read_DidChangeConfigurationClientCapabilities;

   procedure Read_DeclarationLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationLink) renames
     Read_LocationLink;

   procedure Read_Virtual_String_Or_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String_Or_NotebookDocumentFilter) is
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
            Read_NotebookDocumentFilter
              (Handler, Value.NotebookDocumentFilter);
      end case;
   end Read_Virtual_String_Or_NotebookDocumentFilter;

   procedure Read_CallHierarchyItem_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_CallHierarchyItem_Vector (Handler, Value);
      end if;
   end Read_CallHierarchyItem_Vector_Or_Null;

   package DocumentOnTypeFormattingRegistrationOptions_Scope is
      package DocumentOnTypeFormattingRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "firstTriggerCharacter",
         "moreTriggerCharacter"]);

   end DocumentOnTypeFormattingRegistrationOptions_Scope;

   procedure Read_DocumentOnTypeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .DocumentOnTypeFormattingRegistrationOptions) is
      use DocumentOnTypeFormattingRegistrationOptions_Scope;
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
            case DocumentOnTypeFormattingRegistrationOptions_Map.Get_Index
              (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  firstTriggerCharacter
                  Value.firstTriggerCharacter.Clear;
                  Value.firstTriggerCharacter.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  moreTriggerCharacter
                  Read_Virtual_String_Vector
                    (Handler, Value.moreTriggerCharacter);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentOnTypeFormattingRegistrationOptions;

   package DocumentHighlightClientCapabilities_Scope is
      package DocumentHighlightClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end DocumentHighlightClientCapabilities_Scope;

   procedure Read_DocumentHighlightClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightClientCapabilities) is
      use DocumentHighlightClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentHighlightClientCapabilities_Map.Get_Index (Key) is
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
   end Read_DocumentHighlightClientCapabilities;

   procedure Read_Integer_Or_Virtual_String
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Integer_Or_Virtual_String) is
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
   end Read_Integer_Or_Virtual_String;

   procedure Read_AlsReferenceKind_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.AlsReferenceKind_Set) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.AlsReferenceKind_Set renames Value;
         Value : LSP.Enumerations.AlsReferenceKind;
      begin
         Set := (others => False);
         while not Handler.Is_End_Array loop
            Read_AlsReferenceKind (Handler, Value);
            Set (Value) := True;
         end loop;
      end;

      Handler.Read_Next;
   end Read_AlsReferenceKind_Set;

end LSP.Inputs.Part_22;
