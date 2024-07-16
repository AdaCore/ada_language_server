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

package body LSP.Inputs.Part_5 is

   package FoldingRangeClientCapabilities_Scope is
      package FoldingRangeClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "rangeLimit",
         "lineFoldingOnly",
         "foldingRangeKind",
         "foldingRange"]);

      package foldingRange_OfFoldingRangeClientCapabilities_Scope is
         package foldingRange_OfFoldingRangeClientCapabilities_Map is new Minimal_Perfect_Hash
           (["collapsedText"]);

      end foldingRange_OfFoldingRangeClientCapabilities_Scope;

      package foldingRangeKind_OfFoldingRangeClientCapabilities_Scope is
         package foldingRangeKind_OfFoldingRangeClientCapabilities_Map is new Minimal_Perfect_Hash
           (["valueSet"]);

      end foldingRangeKind_OfFoldingRangeClientCapabilities_Scope;

   end FoldingRangeClientCapabilities_Scope;

   procedure Read_FoldingRangeClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeClientCapabilities) is
      use FoldingRangeClientCapabilities_Scope;
      procedure Read_foldingRange_OfFoldingRangeClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .foldingRange_OfFoldingRangeClientCapabilities);

      procedure Read_FoldingRangeKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FoldingRangeKind_Set);

      procedure Read_foldingRangeKind_OfFoldingRangeClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .foldingRangeKind_OfFoldingRangeClientCapabilities);

      procedure Read_foldingRange_OfFoldingRangeClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .foldingRange_OfFoldingRangeClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use foldingRange_OfFoldingRangeClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case foldingRange_OfFoldingRangeClientCapabilities_Map.Get_Index
                 (Key) is
                  when 1 =>  --  collapsedText
                     Value.collapsedText       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.collapsedText.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_foldingRange_OfFoldingRangeClientCapabilities;

      procedure Read_FoldingRangeKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FoldingRangeKind_Set) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FoldingRangeKind_Set renames Value;
            Value : LSP.Enumerations.FoldingRangeKind;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FoldingRangeKind (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FoldingRangeKind_Set;

      procedure Read_foldingRangeKind_OfFoldingRangeClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .foldingRangeKind_OfFoldingRangeClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use foldingRangeKind_OfFoldingRangeClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case foldingRangeKind_OfFoldingRangeClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  valueSet
                     Read_FoldingRangeKind_Set (Handler, Value.valueSet);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_foldingRangeKind_OfFoldingRangeClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FoldingRangeClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  rangeLimit
                  Value.rangeLimit       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.rangeLimit.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  lineFoldingOnly
                  Value.lineFoldingOnly       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.lineFoldingOnly.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  foldingRangeKind
                  Value.foldingRangeKind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_foldingRangeKind_OfFoldingRangeClientCapabilities
                    (Handler, Value.foldingRangeKind.Value);
               when 5 =>  --  foldingRange
                  Value.foldingRange :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_foldingRange_OfFoldingRangeClientCapabilities
                    (Handler, Value.foldingRange.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FoldingRangeClientCapabilities;

   procedure Read_CallHierarchyIncomingCall_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyIncomingCall_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_CallHierarchyIncomingCall_Vector (Handler, Value);
      end if;
   end Read_CallHierarchyIncomingCall_Vector_Or_Null;

   package LinkedEditingRanges_Scope is
      package LinkedEditingRanges_Map is new Minimal_Perfect_Hash
        (["ranges",
         "wordPattern"]);

   end LinkedEditingRanges_Scope;

   procedure Read_LinkedEditingRanges
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRanges) is
      use LinkedEditingRanges_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LinkedEditingRanges_Map.Get_Index (Key) is
               when 1 =>  --  ranges
                  Read_Range_Vector (Handler, Value.ranges);
               when 2 =>  --  wordPattern
                  Value.wordPattern.Clear;
                  Value.wordPattern.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_LinkedEditingRanges;

   procedure Read_Diagnostic_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Diagnostic_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Diagnostic_Vector renames Value;
         Value : LSP.Structures.Diagnostic;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_Diagnostic (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Diagnostic_Vector;

   package NotebookDocumentSyncOptions_Scope is
      package NotebookDocumentSyncOptions_Map is new Minimal_Perfect_Hash
        (["notebookSelector",
         "save"]);

      package cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Scope
      is
         package cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Map is new Minimal_Perfect_Hash
           (["language"]);

      end cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Scope;

      package notebookSelector_OfNotebookDocumentSyncOptions_Item_Scope is
         package notebookSelector_OfNotebookDocumentSyncOptions_Item_Map is new Minimal_Perfect_Hash
           (["notebook",
            "cells"]);

      end notebookSelector_OfNotebookDocumentSyncOptions_Item_Scope;

   end NotebookDocumentSyncOptions_Scope;

   procedure Read_NotebookDocumentSyncOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentSyncOptions) is
      use NotebookDocumentSyncOptions_Scope;
      procedure Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item);

      procedure Read_notebookSelector_OfNotebookDocumentSyncOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions);

      procedure Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item);

      procedure Read_notebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions_Item);

      procedure Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  language
                     Value.language.Clear;
                     Value.language.Append (Handler.String_Value);
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item;

      procedure Read_notebookSelector_OfNotebookDocumentSyncOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   :
              LSP.Structures
                .notebookSelector_OfNotebookDocumentSyncOptions renames
              Value;
            Value :
              LSP.Structures
                .notebookSelector_OfNotebookDocumentSyncOptions_Item;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_notebookSelector_OfNotebookDocumentSyncOptions_Item
                 (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_notebookSelector_OfNotebookDocumentSyncOptions;

      procedure Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   :
              LSP.Structures
                .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item renames
              Value;
            Value :
              LSP.Structures
                .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
                 (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item;

      procedure Read_notebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions_Item) is
         use notebookSelector_OfNotebookDocumentSyncOptions_Item_Scope;
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case notebookSelector_OfNotebookDocumentSyncOptions_Item_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  notebook
                     Value.notebook :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_Virtual_String_Or_NotebookDocumentFilter
                       (Handler, Value.notebook.Value);
                  when 2 =>  --  cells
                     Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
                       (Handler, Value.cells);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_notebookSelector_OfNotebookDocumentSyncOptions_Item;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentSyncOptions_Map.Get_Index (Key) is
               when 1 =>  --  notebookSelector
                  Read_notebookSelector_OfNotebookDocumentSyncOptions
                    (Handler, Value.notebookSelector);
               when 2 =>  --  save
                  Value.save       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.save.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocumentSyncOptions;

   package TypeDefinitionParams_Scope is
      package TypeDefinitionParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken"]);

   end TypeDefinitionParams_Scope;

   procedure Read_TypeDefinitionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionParams) is
      use TypeDefinitionParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeDefinitionParams_Map.Get_Index (Key) is
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
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeDefinitionParams;

   package DeclarationRegistrationOptions_Scope is
      package DeclarationRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "documentSelector",
         "id"]);

   end DeclarationRegistrationOptions_Scope;

   procedure Read_DeclarationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationRegistrationOptions) is
      use DeclarationRegistrationOptions_Scope;
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
            case DeclarationRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_DeclarationRegistrationOptions;

   package SaveOptions_Scope is
      package SaveOptions_Map is new Minimal_Perfect_Hash (["includeText"]);

   end SaveOptions_Scope;

   procedure Read_SaveOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SaveOptions) is
      use SaveOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SaveOptions_Map.Get_Index (Key) is
               when 1 =>  --  includeText
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
   end Read_SaveOptions;

   package ChangeAnnotation_Scope is
      package ChangeAnnotation_Map is new Minimal_Perfect_Hash
        (["label",
         "needsConfirmation",
         "description"]);

   end ChangeAnnotation_Scope;

   procedure Read_ChangeAnnotation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ChangeAnnotation) is
      use ChangeAnnotation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ChangeAnnotation_Map.Get_Index (Key) is
               when 1 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  needsConfirmation
                  Value.needsConfirmation       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.needsConfirmation.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  description
                  Value.description.Clear;
                  Value.description.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ChangeAnnotation;

   package ApplyWorkspaceEditParams_Scope is
      package ApplyWorkspaceEditParams_Map is new Minimal_Perfect_Hash
        (["label",
         "edit"]);

   end ApplyWorkspaceEditParams_Scope;

   procedure Read_ApplyWorkspaceEditParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ApplyWorkspaceEditParams) is
      use ApplyWorkspaceEditParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ApplyWorkspaceEditParams_Map.Get_Index (Key) is
               when 1 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  edit
                  Read_WorkspaceEdit (Handler, Value.edit);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ApplyWorkspaceEditParams;

   package DidCloseTextDocumentParams_Scope is
      package DidCloseTextDocumentParams_Map is new Minimal_Perfect_Hash
        (["textDocument"]);

   end DidCloseTextDocumentParams_Scope;

   procedure Read_DidCloseTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidCloseTextDocumentParams) is
      use DidCloseTextDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidCloseTextDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidCloseTextDocumentParams;

   package PublishDiagnosticsParams_Scope is
      package PublishDiagnosticsParams_Map is new Minimal_Perfect_Hash
        (["uri",
         "version",
         "diagnostics"]);

   end PublishDiagnosticsParams_Scope;

   procedure Read_PublishDiagnosticsParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PublishDiagnosticsParams) is
      use PublishDiagnosticsParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case PublishDiagnosticsParams_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  version
                  Value.version       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.version.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  diagnostics
                  Read_Diagnostic_Vector (Handler, Value.diagnostics);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_PublishDiagnosticsParams;

   procedure Read_TextEdit_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextEdit_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_TextEdit_Vector (Handler, Value);
      end if;
   end Read_TextEdit_Vector_Or_Null;

   package Boolean_Or_Something_Scope is
      package Boolean_Or_Something_Map is new Minimal_Perfect_Hash (["delta"]);

   end Boolean_Or_Something_Scope;

   procedure Read_Boolean_Or_Something
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Boolean_Or_Something) is
      use Boolean_Or_Something_Scope;
   begin
      if Handler.Is_Boolean_Value then
         Value :=
           (Is_Boolean => True,
            others     => <>);
      else
         Value :=
           (Is_Boolean => False,
            others     => <>);
      end if;

      case Value.Is_Boolean is
         when True =>
            Value.Boolean := Handler.Boolean_Value;
            Handler.Read_Next;
         when False =>
            pragma Assert (Handler.Is_Start_Object);
            Handler.Read_Next;

            while Handler.Is_Key_Name loop
               declare
                  Key : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
               begin
                  Handler.Read_Next;
                  case Boolean_Or_Something_Map.Get_Index (Key) is
                     when 1 =>  --  delta
                        Value.a_delta       :=
                          (Is_Set => True,
                           Value  => <>);
                        Value.a_delta.Value := Handler.Boolean_Value;
                        Handler.Read_Next;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;

            Handler.Read_Next;
      end case;
   end Read_Boolean_Or_Something;

   package CallHierarchyOutgoingCall_Scope is
      package CallHierarchyOutgoingCall_Map is new Minimal_Perfect_Hash
        (["to",
         "fromRanges",
         "dispatching_calls"]);

   end CallHierarchyOutgoingCall_Scope;

   procedure Read_CallHierarchyOutgoingCall
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOutgoingCall) is
      use CallHierarchyOutgoingCall_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyOutgoingCall_Map.Get_Index (Key) is
               when 1 =>  --  to
                  Read_CallHierarchyItem (Handler, Value.to);
               when 2 =>  --  fromRanges
                  Read_Range_Vector (Handler, Value.fromRanges);
               when 3 =>  --  dispatching_calls
                  Read_Boolean_Vector (Handler, Value.dispatching_calls);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyOutgoingCall;

   package MarkedString_Scope is
      package MarkedString_Map is new Minimal_Perfect_Hash
        (["language",
         "value"]);

   end MarkedString_Scope;

   procedure Read_MarkedString
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkedString) is
      use MarkedString_Scope;
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
            pragma Assert (Handler.Is_Start_Object);
            Handler.Read_Next;

            while Handler.Is_Key_Name loop
               declare
                  Key : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
               begin
                  Handler.Read_Next;
                  case MarkedString_Map.Get_Index (Key) is
                     when 1 =>  --  language
                        Value.language.Clear;
                        Value.language.Append (Handler.String_Value);
                        Handler.Read_Next;
                     when 2 =>  --  value
                        Value.value.Clear;
                        Value.value.Append (Handler.String_Value);
                        Handler.Read_Next;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;

            Handler.Read_Next;
      end case;
   end Read_MarkedString;

   package ColorInformation_Scope is
      package ColorInformation_Map is new Minimal_Perfect_Hash
        (["range",
         "color"]);

   end ColorInformation_Scope;

   procedure Read_ColorInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorInformation) is
      use ColorInformation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ColorInformation_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  color
                  Read_Color (Handler, Value.color);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ColorInformation;

   package ProgressParams_Scope is
      package ProgressParams_Map is new Minimal_Perfect_Hash
        (["token",
         "value"]);

   end ProgressParams_Scope;

   procedure Read_ProgressParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ProgressParams) is
      use ProgressParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ProgressParams_Map.Get_Index (Key) is
               when 1 =>  --  token
                  Read_ProgressToken (Handler, Value.token);
               when 2 =>  --  value
                  Read_LSPAny (Handler, Value.value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ProgressParams;

end LSP.Inputs.Part_5;
