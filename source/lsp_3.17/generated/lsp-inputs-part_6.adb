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

package body LSP.Inputs.Part_6 is

   package DeclarationOptions_Scope is
      package DeclarationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end DeclarationOptions_Scope;

   procedure Read_DeclarationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationOptions) is
      use DeclarationOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeclarationOptions_Map.Get_Index (Key) is
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
   end Read_DeclarationOptions;

   package SelectionRangeClientCapabilities_Scope is
      package SelectionRangeClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end SelectionRangeClientCapabilities_Scope;

   procedure Read_SelectionRangeClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeClientCapabilities) is
      use SelectionRangeClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SelectionRangeClientCapabilities_Map.Get_Index (Key) is
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
   end Read_SelectionRangeClientCapabilities;

   package DocumentLinkClientCapabilities_Scope is
      package DocumentLinkClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "tooltipSupport"]);

   end DocumentLinkClientCapabilities_Scope;

   procedure Read_DocumentLinkClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkClientCapabilities) is
      use DocumentLinkClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentLinkClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  tooltipSupport
                  Value.tooltipSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.tooltipSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentLinkClientCapabilities;

   package NotebookDocumentSyncRegistrationOptions_Scope is
      package NotebookDocumentSyncRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["notebookSelector",
         "save",
         "id"]);

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

   end NotebookDocumentSyncRegistrationOptions_Scope;

   procedure Read_NotebookDocumentSyncRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentSyncRegistrationOptions) is
      use NotebookDocumentSyncRegistrationOptions_Scope;
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
            case NotebookDocumentSyncRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  notebookSelector
                  Read_notebookSelector_OfNotebookDocumentSyncOptions
                    (Handler, Value.notebookSelector);
               when 2 =>  --  save
                  Value.save       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.save.Value := Handler.Boolean_Value;
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
   end Read_NotebookDocumentSyncRegistrationOptions;

   procedure Read_Hover_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Hover_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_Hover (Handler, Value.Value);
      end if;
   end Read_Hover_Or_Null;

   package TypeHierarchyClientCapabilities_Scope is
      package TypeHierarchyClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end TypeHierarchyClientCapabilities_Scope;

   procedure Read_TypeHierarchyClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyClientCapabilities) is
      use TypeHierarchyClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchyClientCapabilities_Map.Get_Index (Key) is
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
   end Read_TypeHierarchyClientCapabilities;

   package Symbol_Result_Scope is
      package Symbol_Result_Map is new Minimal_Perfect_Hash
        (["deprecated",
         "data"]);

   end Symbol_Result_Scope;

   procedure Read_Symbol_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Symbol_Result) is
      use Symbol_Result_Scope;
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
         if Handler.Is_Start_Array then
            Handler.Read_Next;
         end if;
         if Handler.Is_Null_Value then
            Value :=
              (Kind   => LSP.Structures.Variant_3,
               others => <>);
         elsif Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Symbol_Result_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  deprecated
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  data
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
               Read_SymbolInformation_Vector (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_WorkspaceSymbol_Vector (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               null;  --  #null_value
               Handler.Read_Next;
         end case;
      end;
   end Read_Symbol_Result;

   package MessageActionItem_Scope is
      package MessageActionItem_Map is new Minimal_Perfect_Hash (["title"]);

   end MessageActionItem_Scope;

   procedure Read_MessageActionItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MessageActionItem) is
      use MessageActionItem_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MessageActionItem_Map.Get_Index (Key) is
               when 1 =>  --  title
                  Value.title.Clear;
                  Value.title.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_MessageActionItem;

   package WorkspaceFullDocumentDiagnosticReport_Scope is
      package WorkspaceFullDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "resultId",
         "items",
         "uri",
         "version"]);

   end WorkspaceFullDocumentDiagnosticReport_Scope;

   procedure Read_WorkspaceFullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFullDocumentDiagnosticReport) is
      use WorkspaceFullDocumentDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceFullDocumentDiagnosticReport_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: full
               when 2 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  items
                  Read_Diagnostic_Vector (Handler, Value.items);
               when 4 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 5 =>  --  version
                  Read_Integer_Or_Null (Handler, Value.version);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceFullDocumentDiagnosticReport;

   package SemanticTokensDeltaPartialResult_Scope is
      package SemanticTokensDeltaPartialResult_Map is new Minimal_Perfect_Hash
        (["edits"]);

   end SemanticTokensDeltaPartialResult_Scope;

   procedure Read_SemanticTokensDeltaPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensDeltaPartialResult) is
      use SemanticTokensDeltaPartialResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensDeltaPartialResult_Map.Get_Index (Key) is
               when 1 =>  --  edits
                  Read_SemanticTokensEdit_Vector (Handler, Value.edits);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensDeltaPartialResult;

   package ShowDocumentParams_Scope is
      package ShowDocumentParams_Map is new Minimal_Perfect_Hash
        (["uri",
         "external",
         "takeFocus",
         "selection"]);

   end ShowDocumentParams_Scope;

   procedure Read_ShowDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowDocumentParams) is
      use ShowDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ShowDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Read_URI (Handler, Value.uri);
               when 2 =>  --  external
                  Value.external       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.external.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  takeFocus
                  Value.takeFocus       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.takeFocus.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  selection
                  Value.selection :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_A_Range (Handler, Value.selection.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ShowDocumentParams;

   package DeleteFilesParams_Scope is
      package DeleteFilesParams_Map is new Minimal_Perfect_Hash (["files"]);

   end DeleteFilesParams_Scope;

   procedure Read_DeleteFilesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeleteFilesParams) is
      use DeleteFilesParams_Scope;
      procedure Read_FileDelete_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileDelete_Vector);

      procedure Read_FileDelete_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileDelete_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FileDelete_Vector renames Value;
            Value : LSP.Structures.FileDelete;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FileDelete (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FileDelete_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeleteFilesParams_Map.Get_Index (Key) is
               when 1 =>  --  files
                  Read_FileDelete_Vector (Handler, Value.files);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DeleteFilesParams;

   procedure Read_InitializedParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializedParams) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            Handler.Skip_Current_Value;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InitializedParams;

   package NotebookCellArrayChange_Scope is
      package NotebookCellArrayChange_Map is new Minimal_Perfect_Hash
        (["start",
         "deleteCount",
         "cells"]);

   end NotebookCellArrayChange_Scope;

   procedure Read_NotebookCellArrayChange
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCellArrayChange) is
      use NotebookCellArrayChange_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookCellArrayChange_Map.Get_Index (Key) is
               when 1 =>  --  start
                  Value.start := Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  deleteCount
                  Value.deleteCount :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  cells
                  Read_NotebookCell_Vector (Handler, Value.cells);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookCellArrayChange;

   package InitializeError_Scope is
      package InitializeError_Map is new Minimal_Perfect_Hash (["retry"]);

   end InitializeError_Scope;

   procedure Read_InitializeError
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializeError) is
      use InitializeError_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InitializeError_Map.Get_Index (Key) is
               when 1 =>  --  retry
                  Value.retry := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InitializeError;

   procedure Read_Integer_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Integer_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value       :=
           (Is_Null => False,
            Value   => <>);
         Value.Value := Integer (Handler.Number_Value.Integer_Value);
         Handler.Read_Next;
      end if;
   end Read_Integer_Or_Null;

end LSP.Inputs.Part_6;
