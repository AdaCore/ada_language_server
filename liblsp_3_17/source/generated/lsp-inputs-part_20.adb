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

package body LSP.Inputs.Part_20 is

   package Definition_Progress_Report_Scope is
      package Definition_Progress_Report_Map is new Minimal_Perfect_Hash
        (["uri",
         "range",
         "alsKind",
         "originSelectionRange",
         "targetUri",
         "targetRange",
         "targetSelectionRange"]);

   end Definition_Progress_Report_Scope;

   procedure Read_Definition_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Definition_Progress_Report) is
      use Definition_Progress_Report_Scope;
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
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Definition_Progress_Report_Map.Get_Index (Key);
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
                     when 4 =>  --  originSelectionRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 5 =>  --  targetUri
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 6 =>  --  targetRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 7 =>  --  targetSelectionRange
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
               Read_Location_Vector (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_DefinitionLink_Vector (Handler, Value.Variant_2);
         end case;
      end;
   end Read_Definition_Progress_Report;

   package NotebookDocumentFilter_Scope is
      package NotebookDocumentFilter_Map is new Minimal_Perfect_Hash
        (["notebookType",
         "scheme",
         "pattern"]);

   end NotebookDocumentFilter_Scope;

   procedure Read_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentFilter) is
      use NotebookDocumentFilter_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentFilter_Map.Get_Index (Key) is
               when 1 =>  --  notebookType
                  Value.notebookType.Clear;
                  Value.notebookType.Append (Handler.String_Value);
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
   end Read_NotebookDocumentFilter;

   procedure Read_ErrorCodes
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.ErrorCodes) is
   begin
      Value :=
        LSP.Enumerations.ErrorCodes (Handler.Number_Value.Integer_Value);
      Handler.Read_Next;
   end Read_ErrorCodes;

   package WorkspaceEditClientCapabilities_Scope is
      package WorkspaceEditClientCapabilities_Map is new Minimal_Perfect_Hash
        (["documentChanges",
         "resourceOperations",
         "failureHandling",
         "normalizesLineEndings",
         "changeAnnotationSupport"]);

      package changeAnnotationSupport_OfWorkspaceEditClientCapabilities_Scope
      is
         package changeAnnotationSupport_OfWorkspaceEditClientCapabilities_Map is new Minimal_Perfect_Hash
           (["groupsOnLabel"]);

      end changeAnnotationSupport_OfWorkspaceEditClientCapabilities_Scope;

   end WorkspaceEditClientCapabilities_Scope;

   procedure Read_WorkspaceEditClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceEditClientCapabilities) is
      use WorkspaceEditClientCapabilities_Scope;
      procedure Read_ResourceOperationKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.ResourceOperationKind_Set);

      procedure Read_changeAnnotationSupport_OfWorkspaceEditClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .changeAnnotationSupport_OfWorkspaceEditClientCapabilities);

      procedure Read_ResourceOperationKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.ResourceOperationKind_Set) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.ResourceOperationKind_Set renames Value;
            Value : LSP.Enumerations.ResourceOperationKind;
         begin
            Set := (others => False);
            while not Handler.Is_End_Array loop
               Read_ResourceOperationKind (Handler, Value);
               Set (Value) := True;
            end loop;
         end;

         Handler.Read_Next;
      end Read_ResourceOperationKind_Set;

      procedure Read_changeAnnotationSupport_OfWorkspaceEditClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .changeAnnotationSupport_OfWorkspaceEditClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 changeAnnotationSupport_OfWorkspaceEditClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case changeAnnotationSupport_OfWorkspaceEditClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  groupsOnLabel
                     Value.groupsOnLabel       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.groupsOnLabel.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_changeAnnotationSupport_OfWorkspaceEditClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceEditClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  documentChanges
                  Value.documentChanges       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.documentChanges.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resourceOperations
                  Read_ResourceOperationKind_Set
                    (Handler, Value.resourceOperations);
               when 3 =>  --  failureHandling
                  Value.failureHandling :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FailureHandlingKind
                    (Handler, Value.failureHandling.Value);
               when 4 =>  --  normalizesLineEndings
                  Value.normalizesLineEndings       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.normalizesLineEndings.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  changeAnnotationSupport
                  Value.changeAnnotationSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_changeAnnotationSupport_OfWorkspaceEditClientCapabilities
                    (Handler, Value.changeAnnotationSupport.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceEditClientCapabilities;

   procedure Read_MessageActionItem_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MessageActionItem_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_MessageActionItem (Handler, Value.Value);
      end if;
   end Read_MessageActionItem_Or_Null;

   package SemanticTokensDeltaParams_Scope is
      package SemanticTokensDeltaParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "previousResultId"]);

   end SemanticTokensDeltaParams_Scope;

   procedure Read_SemanticTokensDeltaParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensDeltaParams) is
      use SemanticTokensDeltaParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensDeltaParams_Map.Get_Index (Key) is
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
               when 4 =>  --  previousResultId
                  Value.previousResultId.Clear;
                  Value.previousResultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensDeltaParams;

   package DocumentDiagnosticReport_Scope is
      package DocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["full",
         "unchanged"]);

   end DocumentDiagnosticReport_Scope;

   procedure Read_DocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentDiagnosticReport) is
      use DocumentDiagnosticReport_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
         Kind    : Natural;
      begin
         Handler.Mark;
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;
         while Handler.Is_Key_Name loop
            declare
               use type VSS.Strings.Virtual_String;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               if Key = "kind" then
                  pragma Assert (Handler.Is_String_Value);
                  Kind :=
                    DocumentDiagnosticReport_Map.Get_Index
                      (Handler.String_Value);
                  case Kind is
                     when 1 =>  --  full
                        Value :=
                          (Kind   => LSP.Structures.full,
                           others => <>);
                     when 2 =>  --  unchanged
                        Value :=
                          (Kind   => LSP.Structures.unchanged,
                           others => <>);
                     when others =>
                        raise Constraint_Error;
                  end case;
                  exit;
               else
                  Handler.Skip_Current_Value;
               end if;
            end;
         end loop;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.full =>
               Read_RelatedFullDocumentDiagnosticReport (Handler, Value.full);
            when LSP.Structures.unchanged =>
               Read_RelatedUnchangedDocumentDiagnosticReport
                 (Handler, Value.unchanged);
         end case;
      end;
   end Read_DocumentDiagnosticReport;

   package MonikerClientCapabilities_Scope is
      package MonikerClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end MonikerClientCapabilities_Scope;

   procedure Read_MonikerClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerClientCapabilities) is
      use MonikerClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MonikerClientCapabilities_Map.Get_Index (Key) is
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
   end Read_MonikerClientCapabilities;

   package RenameFileOptions_Scope is
      package RenameFileOptions_Map is new Minimal_Perfect_Hash
        (["overwrite",
         "ignoreIfExists"]);

   end RenameFileOptions_Scope;

   procedure Read_RenameFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameFileOptions) is
      use RenameFileOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameFileOptions_Map.Get_Index (Key) is
               when 1 =>  --  overwrite
                  Value.overwrite       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.overwrite.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  ignoreIfExists
                  Value.ignoreIfExists       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.ignoreIfExists.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameFileOptions;

   package DocumentSymbolClientCapabilities_Scope is
      package DocumentSymbolClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "symbolKind",
         "hierarchicalDocumentSymbolSupport",
         "tagSupport",
         "labelSupport"]);

   end DocumentSymbolClientCapabilities_Scope;

   procedure Read_DocumentSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolClientCapabilities) is
      use DocumentSymbolClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentSymbolClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  symbolKind
                  Value.symbolKind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_symbolKind_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.symbolKind.Value);
               when 3 =>  --  hierarchicalDocumentSymbolSupport
                  Value.hierarchicalDocumentSymbolSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.hierarchicalDocumentSymbolSupport.Value :=
                    Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  tagSupport
                  Value.tagSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_tagSupport_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.tagSupport.Value);
               when 5 =>  --  labelSupport
                  Value.labelSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.labelSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentSymbolClientCapabilities;

   package Hover_Scope is
      package Hover_Map is new Minimal_Perfect_Hash
        (["contents",
         "range"]);

      package MarkupContent_Or_MarkedString_Vector_Scope is
         package MarkupContent_Or_MarkedString_Vector_Map is new Minimal_Perfect_Hash
           (["kind",
            "language"]);

      end MarkupContent_Or_MarkedString_Vector_Scope;

   end Hover_Scope;

   procedure Read_Hover
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Hover) is
      use Hover_Scope;
      procedure Read_MarkedString_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.MarkedString_Vector);

      procedure Read_MarkupContent_Or_MarkedString_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.MarkupContent_Or_MarkedString_Vector);

      procedure Read_MarkedString_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.MarkedString_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.MarkedString_Vector renames Value;
            Value : LSP.Structures.MarkedString;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_MarkedString (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_MarkedString_Vector;

      procedure Read_MarkupContent_Or_MarkedString_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.MarkupContent_Or_MarkedString_Vector) is
      --  use MarkupContent_Or_MarkedString_Vector_Scope;
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
            LSP.Input_Tools.Look_For_MarkupContent_Or_MarkedString_Vector
              (Handler, Value);
            Handler.Reset;
            Handler.Unmark;
            if not Value.Is_MarkupContent and not Handler.Is_Start_Array then
               Read_MarkedString (Handler, Value.MarkedString_Vector (1));
               return;
            end if;

            case Value.Is_MarkupContent is
               when True =>
                  Read_MarkupContent (Handler, Value.MarkupContent);
               when False =>
                  Read_MarkedString_Vector
                    (Handler, Value.MarkedString_Vector);
            end case;
         end;
      end Read_MarkupContent_Or_MarkedString_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Hover_Map.Get_Index (Key) is
               when 1 =>  --  contents
                  Read_MarkupContent_Or_MarkedString_Vector
                    (Handler, Value.contents);
               when 2 =>  --  range
                  Value.a_range :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_A_Range (Handler, Value.a_range.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Hover;

end LSP.Inputs.Part_20;
