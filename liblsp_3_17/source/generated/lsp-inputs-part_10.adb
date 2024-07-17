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

package body LSP.Inputs.Part_10 is

   procedure Read_SymbolInformation_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolInformation_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.SymbolInformation_Vector renames Value;
         Value : LSP.Structures.SymbolInformation;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_SymbolInformation (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_SymbolInformation_Vector;

   package CodeActionContext_Scope is
      package CodeActionContext_Map is new Minimal_Perfect_Hash
        (["diagnostics",
         "only",
         "triggerKind"]);

   end CodeActionContext_Scope;

   procedure Read_CodeActionContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionContext) is
      use CodeActionContext_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeActionContext_Map.Get_Index (Key) is
               when 1 =>  --  diagnostics
                  Read_Diagnostic_Vector (Handler, Value.diagnostics);
               when 2 =>  --  only
                  Read_CodeActionKind_Set (Handler, Value.only);
               when 3 =>  --  triggerKind
                  Value.triggerKind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeActionTriggerKind
                    (Handler, Value.triggerKind.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeActionContext;

   package RenameParams_Scope is
      package RenameParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "textDocument",
         "position",
         "newName"]);

   end RenameParams_Scope;

   procedure Read_RenameParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameParams) is
      use RenameParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 3 =>  --  position
                  Read_Position (Handler, Value.position);
               when 4 =>  --  newName
                  Value.newName.Clear;
                  Value.newName.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameParams;

   package CodeActionClientCapabilities_Scope is
      package CodeActionClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "codeActionLiteralSupport",
         "isPreferredSupport",
         "disabledSupport",
         "dataSupport",
         "resolveSupport",
         "honorsChangeAnnotations"]);

      package codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities_Scope
      is
         package codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["valueSet"]);

      end codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities_Scope;

      package codeActionLiteralSupport_OfCodeActionClientCapabilities_Scope is
         package codeActionLiteralSupport_OfCodeActionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["codeActionKind"]);

      end codeActionLiteralSupport_OfCodeActionClientCapabilities_Scope;

   end CodeActionClientCapabilities_Scope;

   procedure Read_CodeActionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionClientCapabilities) is
      use CodeActionClientCapabilities_Scope;
      procedure Read_codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities);

      procedure Read_codeActionLiteralSupport_OfCodeActionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .codeActionLiteralSupport_OfCodeActionClientCapabilities);

      procedure Read_codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  valueSet
                     Read_CodeActionKind_Set (Handler, Value.valueSet);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities;

      procedure Read_codeActionLiteralSupport_OfCodeActionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .codeActionLiteralSupport_OfCodeActionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 codeActionLiteralSupport_OfCodeActionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case codeActionLiteralSupport_OfCodeActionClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  codeActionKind
                     Read_codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities
                       (Handler, Value.codeActionKind);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_codeActionLiteralSupport_OfCodeActionClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeActionClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  codeActionLiteralSupport
                  Value.codeActionLiteralSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_codeActionLiteralSupport_OfCodeActionClientCapabilities
                    (Handler, Value.codeActionLiteralSupport.Value);
               when 3 =>  --  isPreferredSupport
                  Value.isPreferredSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.isPreferredSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  disabledSupport
                  Value.disabledSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.disabledSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  dataSupport
                  Value.dataSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dataSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  resolveSupport
                  Value.resolveSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_resolveSupport_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.resolveSupport.Value);
               when 7 =>  --  honorsChangeAnnotations
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
   end Read_CodeActionClientCapabilities;

   package NotebookDocumentChangeEvent_Scope is
      package NotebookDocumentChangeEvent_Map is new Minimal_Perfect_Hash
        (["metadata",
         "cells"]);

      package textContent_Ofcells_OfNotebookDocumentChangeEvent_Item_Scope is
         package textContent_Ofcells_OfNotebookDocumentChangeEvent_Item_Map is new Minimal_Perfect_Hash
           (["document",
            "changes"]);

      end textContent_Ofcells_OfNotebookDocumentChangeEvent_Item_Scope;

      package cells_OfNotebookDocumentChangeEvent_Scope is
         package cells_OfNotebookDocumentChangeEvent_Map is new Minimal_Perfect_Hash
           (["structure",
            "data",
            "textContent"]);

      end cells_OfNotebookDocumentChangeEvent_Scope;

      package structure_Ofcells_OfNotebookDocumentChangeEvent_Scope is
         package structure_Ofcells_OfNotebookDocumentChangeEvent_Map is new Minimal_Perfect_Hash
           (["array",
            "didOpen",
            "didClose"]);

      end structure_Ofcells_OfNotebookDocumentChangeEvent_Scope;

   end NotebookDocumentChangeEvent_Scope;

   procedure Read_NotebookDocumentChangeEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentChangeEvent) is
      use NotebookDocumentChangeEvent_Scope;
      procedure Read_textContent_Ofcells_OfNotebookDocumentChangeEvent_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .textContent_Ofcells_OfNotebookDocumentChangeEvent_Item);

      procedure Read_cells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.cells_OfNotebookDocumentChangeEvent);

      procedure Read_textContent_Ofcells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .textContent_Ofcells_OfNotebookDocumentChangeEvent);

      procedure Read_structure_Ofcells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .structure_Ofcells_OfNotebookDocumentChangeEvent);

      procedure Read_textContent_Ofcells_OfNotebookDocumentChangeEvent_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .textContent_Ofcells_OfNotebookDocumentChangeEvent_Item) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 textContent_Ofcells_OfNotebookDocumentChangeEvent_Item_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case textContent_Ofcells_OfNotebookDocumentChangeEvent_Item_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  document
                     Read_VersionedTextDocumentIdentifier
                       (Handler, Value.document);
                  when 2 =>  --  changes
                     Read_TextDocumentContentChangeEvent_Vector
                       (Handler, Value.changes);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_textContent_Ofcells_OfNotebookDocumentChangeEvent_Item;

      procedure Read_cells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.cells_OfNotebookDocumentChangeEvent) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use cells_OfNotebookDocumentChangeEvent_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case cells_OfNotebookDocumentChangeEvent_Map.Get_Index (Key) is
                  when 1 =>  --  structure
                     Value.structure :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_structure_Ofcells_OfNotebookDocumentChangeEvent
                       (Handler, Value.structure.Value);
                  when 2 =>  --  data
                     Read_NotebookCell_Vector (Handler, Value.data);
                  when 3 =>  --  textContent
                     Read_textContent_Ofcells_OfNotebookDocumentChangeEvent
                       (Handler, Value.textContent);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_cells_OfNotebookDocumentChangeEvent;

      procedure Read_textContent_Ofcells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .textContent_Ofcells_OfNotebookDocumentChangeEvent) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   :
              LSP.Structures
                .textContent_Ofcells_OfNotebookDocumentChangeEvent renames
              Value;
            Value :
              LSP.Structures
                .textContent_Ofcells_OfNotebookDocumentChangeEvent_Item;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_textContent_Ofcells_OfNotebookDocumentChangeEvent_Item
                 (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_textContent_Ofcells_OfNotebookDocumentChangeEvent;

      procedure Read_structure_Ofcells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .structure_Ofcells_OfNotebookDocumentChangeEvent) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use structure_Ofcells_OfNotebookDocumentChangeEvent_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case structure_Ofcells_OfNotebookDocumentChangeEvent_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  array
                     Read_NotebookCellArrayChange (Handler, Value.an_array);
                  when 2 =>  --  didOpen
                     Read_TextDocumentItem_Vector (Handler, Value.didOpen);
                  when 3 =>  --  didClose
                     Read_TextDocumentIdentifier_Vector
                       (Handler, Value.didClose);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_structure_Ofcells_OfNotebookDocumentChangeEvent;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentChangeEvent_Map.Get_Index (Key) is
               when 1 =>  --  metadata
                  Value.metadata :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_LSPObject (Handler, Value.metadata.Value);
               when 2 =>  --  cells
                  Value.cells :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_cells_OfNotebookDocumentChangeEvent
                    (Handler, Value.cells.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocumentChangeEvent;

   package TypeDefinitionRegistrationOptions_Scope is
      package TypeDefinitionRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end TypeDefinitionRegistrationOptions_Scope;

   procedure Read_TypeDefinitionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionRegistrationOptions) is
      use TypeDefinitionRegistrationOptions_Scope;
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
            case TypeDefinitionRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_TypeDefinitionRegistrationOptions;

   procedure Read_TypeHierarchyItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyItem_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.TypeHierarchyItem_Vector renames Value;
         Value : LSP.Structures.TypeHierarchyItem;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_TypeHierarchyItem (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_TypeHierarchyItem_Vector;

   package DocumentDiagnosticReportPartialResult_Scope is
      package DocumentDiagnosticReportPartialResult_Map is new Minimal_Perfect_Hash
        (["relatedDocuments"]);

   end DocumentDiagnosticReportPartialResult_Scope;

   procedure Read_DocumentDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentDiagnosticReportPartialResult) is
      use DocumentDiagnosticReportPartialResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentDiagnosticReportPartialResult_Map.Get_Index (Key) is
               when 1 =>  --  relatedDocuments
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
   end Read_DocumentDiagnosticReportPartialResult;

end LSP.Inputs.Part_10;
