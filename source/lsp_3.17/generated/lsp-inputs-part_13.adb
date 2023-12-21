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

package body LSP.Inputs.Part_13 is

   procedure Read_Null_Record
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Null_Record) is
   begin
      null;  --  #null_value
      Handler.Read_Next;
   end Read_Null_Record;

   procedure Read_NotebookCell_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCell_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.NotebookCell_Vector renames Value;
         Value : LSP.Structures.NotebookCell;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_NotebookCell (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_NotebookCell_Vector;

   procedure Read_LSPAny
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPAny) is
   begin
      LSP.Input_Tools.Read_LSPAny_Class (Handler, Value);
   end Read_LSPAny;

   procedure Read_SymbolTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolTag_Set) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.SymbolTag_Set renames Value;
         Value : LSP.Enumerations.SymbolTag;
      begin
         Set := (others => False);
         while not Handler.Is_End_Array loop
            Read_SymbolTag (Handler, Value);
            Set (Value) := True;
         end loop;
      end;

      Handler.Read_Next;
   end Read_SymbolTag_Set;

   package GeneralClientCapabilities_Scope is
      package GeneralClientCapabilities_Map is new Minimal_Perfect_Hash
        (["staleRequestSupport",
         "regularExpressions",
         "markdown",
         "positionEncodings"]);

      package staleRequestSupport_OfGeneralClientCapabilities_Scope is
         package staleRequestSupport_OfGeneralClientCapabilities_Map is new Minimal_Perfect_Hash
           (["cancel",
            "retryOnContentModified"]);

      end staleRequestSupport_OfGeneralClientCapabilities_Scope;

   end GeneralClientCapabilities_Scope;

   procedure Read_GeneralClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.GeneralClientCapabilities) is
      use GeneralClientCapabilities_Scope;
      procedure Read_PositionEncodingKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PositionEncodingKind_Set);

      procedure Read_staleRequestSupport_OfGeneralClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .staleRequestSupport_OfGeneralClientCapabilities);

      procedure Read_PositionEncodingKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PositionEncodingKind_Set) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.PositionEncodingKind_Set renames Value;
            Value : LSP.Enumerations.PositionEncodingKind;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_PositionEncodingKind (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_PositionEncodingKind_Set;

      procedure Read_staleRequestSupport_OfGeneralClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .staleRequestSupport_OfGeneralClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use staleRequestSupport_OfGeneralClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case staleRequestSupport_OfGeneralClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  cancel
                     Value.cancel := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when 2 =>  --  retryOnContentModified
                     Read_Virtual_String_Vector
                       (Handler, Value.retryOnContentModified);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_staleRequestSupport_OfGeneralClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case GeneralClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  staleRequestSupport
                  Value.staleRequestSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_staleRequestSupport_OfGeneralClientCapabilities
                    (Handler, Value.staleRequestSupport.Value);
               when 2 =>  --  regularExpressions
                  Value.regularExpressions :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_RegularExpressionsClientCapabilities
                    (Handler, Value.regularExpressions.Value);
               when 3 =>  --  markdown
                  Value.markdown :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_MarkdownClientCapabilities
                    (Handler, Value.markdown.Value);
               when 4 =>  --  positionEncodings
                  Read_PositionEncodingKind_Set
                    (Handler, Value.positionEncodings);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_GeneralClientCapabilities;

   procedure Read_CodeLens_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLens_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_CodeLens_Vector (Handler, Value);
      end if;
   end Read_CodeLens_Vector_Or_Null;

   package HoverClientCapabilities_Scope is
      package HoverClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "contentFormat"]);

   end HoverClientCapabilities_Scope;

   procedure Read_HoverClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverClientCapabilities) is
      use HoverClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case HoverClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  contentFormat
                  Read_MarkupKind_Vector (Handler, Value.contentFormat);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_HoverClientCapabilities;

   procedure Read_InsertTextFormat
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InsertTextFormat) is
   begin
      Value :=
        LSP.Enumerations.InsertTextFormat'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_InsertTextFormat;

   package DocumentSymbolOptions_Scope is
      package DocumentSymbolOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "label"]);

   end DocumentSymbolOptions_Scope;

   procedure Read_DocumentSymbolOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolOptions) is
      use DocumentSymbolOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentSymbolOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentSymbolOptions;

   package Tokens_Delta_Result_Scope is
      package Tokens_Delta_Result_Map is new Minimal_Perfect_Hash
        (["data",
         "edits"]);

   end Tokens_Delta_Result_Scope;

   procedure Read_Tokens_Delta_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Tokens_Delta_Result) is
      use Tokens_Delta_Result_Scope;
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
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Tokens_Delta_Result_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  data
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  edits
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
               Read_SemanticTokens (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_SemanticTokensDelta (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               null;  --  #null_value
               Handler.Read_Next;
         end case;
      end;
   end Read_Tokens_Delta_Result;

   package MonikerOptions_Scope is
      package MonikerOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end MonikerOptions_Scope;

   procedure Read_MonikerOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerOptions) is
      use MonikerOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MonikerOptions_Map.Get_Index (Key) is
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
   end Read_MonikerOptions;

   package TypeHierarchySupertypesParams_Scope is
      package TypeHierarchySupertypesParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "item"]);

   end TypeHierarchySupertypesParams_Scope;

   procedure Read_TypeHierarchySupertypesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchySupertypesParams) is
      use TypeHierarchySupertypesParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchySupertypesParams_Map.Get_Index (Key) is
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
                  Read_TypeHierarchyItem (Handler, Value.item);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeHierarchySupertypesParams;

   package Moniker_Scope is
      package Moniker_Map is new Minimal_Perfect_Hash
        (["scheme",
         "identifier",
         "unique",
         "kind"]);

   end Moniker_Scope;

   procedure Read_Moniker
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Moniker) is
      use Moniker_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Moniker_Map.Get_Index (Key) is
               when 1 =>  --  scheme
                  Value.scheme.Clear;
                  Value.scheme.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  identifier
                  Value.identifier.Clear;
                  Value.identifier.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  unique
                  Read_UniquenessLevel (Handler, Value.unique);
               when 4 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_MonikerKind (Handler, Value.kind.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Moniker;

   package MarkupContent_Scope is
      package MarkupContent_Map is new Minimal_Perfect_Hash
        (["kind",
         "value"]);

   end MarkupContent_Scope;

   procedure Read_MarkupContent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkupContent) is
      use MarkupContent_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MarkupContent_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Read_MarkupKind (Handler, Value.kind);
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
   end Read_MarkupContent;

   package WorkspaceEdit_Scope is
      package WorkspaceEdit_Map is new Minimal_Perfect_Hash
        (["changes",
         "documentChanges",
         "changeAnnotations"]);

      package documentChanges_OfWorkspaceEdit_Item_Scope is
         package documentChanges_OfWorkspaceEdit_Item_Map is new Minimal_Perfect_Hash
           (["create",
            "rename",
            "delete"]);

      end documentChanges_OfWorkspaceEdit_Item_Scope;

   end WorkspaceEdit_Scope;

   procedure Read_WorkspaceEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceEdit) is
      use WorkspaceEdit_Scope;
      procedure Read_changes_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.changes_OfWorkspaceEdit);

      procedure Read_documentChanges_OfWorkspaceEdit_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.documentChanges_OfWorkspaceEdit_Item);

      procedure Read_changeAnnotations_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.changeAnnotations_OfWorkspaceEdit);

      procedure Read_documentChanges_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.documentChanges_OfWorkspaceEdit);

      procedure Read_changes_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.changes_OfWorkspaceEdit) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while not Handler.Is_End_Object loop
            declare
               Map   : LSP.Structures.changes_OfWorkspaceEdit renames Value;
               Key   : LSP.Structures.DocumentUri;
               Value : LSP.Structures.TextEdit_Vector;
            begin
               Key := (Handler.Key_Name with null record);
               Handler.Read_Next;
               Read_TextEdit_Vector (Handler, Value);
               Map.Insert (Key, Value);
            end;
         end loop;

         Handler.Read_Next;

      end Read_changes_OfWorkspaceEdit;

      procedure Read_documentChanges_OfWorkspaceEdit_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.documentChanges_OfWorkspaceEdit_Item) is
         use documentChanges_OfWorkspaceEdit_Item_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
            Kind    : Natural;
         begin
            Handler.Mark;
            pragma Assert (Handler.Is_Start_Object);
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  use type VSS.Strings.Virtual_String;
                  Key : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
               begin
                  Handler.Read_Next;
                  if Key = "kind" then
                     pragma Assert (Handler.Is_String_Value);
                     Kind :=
                       documentChanges_OfWorkspaceEdit_Item_Map.Get_Index
                         (Handler.String_Value);
                     case Kind is
                        when 1 =>  --  create
                           Value :=
                             (Kind   => LSP.Structures.create,
                              others => <>);
                        when 2 =>  --  rename
                           Value :=
                             (Kind   => LSP.Structures.rename,
                              others => <>);
                        when 3 =>  --  delete
                           Value :=
                             (Kind   => LSP.Structures.delete,
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
               when LSP.Structures.Variant_1 =>
                  Read_TextDocumentEdit (Handler, Value.Variant_1);
               when LSP.Structures.create =>
                  Read_CreateFile (Handler, Value.create);
               when LSP.Structures.rename =>
                  Read_RenameFile (Handler, Value.rename);
               when LSP.Structures.delete =>
                  Read_DeleteFile (Handler, Value.delete);
            end case;
         end;
      end Read_documentChanges_OfWorkspaceEdit_Item;

      procedure Read_changeAnnotations_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.changeAnnotations_OfWorkspaceEdit) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while not Handler.Is_End_Object loop
            declare
               Map   :
                 LSP.Structures.changeAnnotations_OfWorkspaceEdit renames
                 Value;
               Key   : LSP.Structures.ChangeAnnotationIdentifier;
               Value : LSP.Structures.ChangeAnnotation;
            begin
               Key := (Handler.Key_Name with null record);
               Handler.Read_Next;
               Read_ChangeAnnotation (Handler, Value);
               Map.Insert (Key, Value);
            end;
         end loop;

         Handler.Read_Next;

      end Read_changeAnnotations_OfWorkspaceEdit;

      procedure Read_documentChanges_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.documentChanges_OfWorkspaceEdit) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set : LSP.Structures.documentChanges_OfWorkspaceEdit renames Value;
            Value : LSP.Structures.documentChanges_OfWorkspaceEdit_Item;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_documentChanges_OfWorkspaceEdit_Item (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_documentChanges_OfWorkspaceEdit;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceEdit_Map.Get_Index (Key) is
               when 1 =>  --  changes
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while not Handler.Is_End_Object loop
                     declare
                        Map   :
                          LSP.Structures.changes_OfWorkspaceEdit renames
                          Value.changes;
                        Key   : LSP.Structures.DocumentUri;
                        Value : LSP.Structures.TextEdit_Vector;
                     begin
                        Key := (Handler.Key_Name with null record);
                        Handler.Read_Next;
                        Read_TextEdit_Vector (Handler, Value);
                        Map.Insert (Key, Value);
                     end;
                  end loop;

                  Handler.Read_Next;

               when 2 =>  --  documentChanges
                  Read_documentChanges_OfWorkspaceEdit
                    (Handler, Value.documentChanges);
               when 3 =>  --  changeAnnotations
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while not Handler.Is_End_Object loop
                     declare
                        Map   :
                          LSP.Structures
                            .changeAnnotations_OfWorkspaceEdit renames
                          Value.changeAnnotations;
                        Key   : LSP.Structures.ChangeAnnotationIdentifier;
                        Value : LSP.Structures.ChangeAnnotation;
                     begin
                        Key := (Handler.Key_Name with null record);
                        Handler.Read_Next;
                        Read_ChangeAnnotation (Handler, Value);
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
   end Read_WorkspaceEdit;

   procedure Read_InsertTextMode
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InsertTextMode) is
   begin
      Value :=
        LSP.Enumerations.InsertTextMode'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_InsertTextMode;

   procedure Read_CompletionItemKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionItemKind) is
   begin
      Value :=
        LSP.Enumerations.CompletionItemKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_CompletionItemKind;

   package Command_Or_CodeAction_Scope is
      package Command_Or_CodeAction_Map is new Minimal_Perfect_Hash
        (["arguments",
         "kind",
         "diagnostics",
         "isPreferred",
         "disabled",
         "edit",
         "data"]);

   end Command_Or_CodeAction_Scope;

   procedure Read_Command_Or_CodeAction
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command_Or_CodeAction) is
      use Command_Or_CodeAction_Scope;
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
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Command_Or_CodeAction_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  arguments
                        Value :=
                          (Is_Command => True,
                           others     => <>);
                        exit;
                     when 2 =>  --  kind
                        Value :=
                          (Is_Command => False,
                           others     => <>);
                        exit;
                     when 3 =>  --  diagnostics
                        Value :=
                          (Is_Command => False,
                           others     => <>);
                        exit;
                     when 4 =>  --  isPreferred
                        Value :=
                          (Is_Command => False,
                           others     => <>);
                        exit;
                     when 5 =>  --  disabled
                        Value :=
                          (Is_Command => False,
                           others     => <>);
                        exit;
                     when 6 =>  --  edit
                        Value :=
                          (Is_Command => False,
                           others     => <>);
                        exit;
                     when 7 =>  --  data
                        Value :=
                          (Is_Command => False,
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

         case Value.Is_Command is
            when True =>
               Read_Command (Handler, Value.Command);
            when False =>
               Read_CodeAction (Handler, Value.CodeAction);
         end case;
      end;
   end Read_Command_Or_CodeAction;

   procedure Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         declare
            Map   :
              LSP.Structures
                .relatedDocuments_OfDocumentDiagnosticReportPartialResult renames
              Value;
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

   end Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult;

   package DocumentSymbol_Scope is
      package DocumentSymbol_Map is new Minimal_Perfect_Hash
        (["name",
         "detail",
         "kind",
         "tags",
         "deprecated",
         "range",
         "selectionRange",
         "children",
         "alsIsDeclaration",
         "alsIsAdaProcedure",
         "alsVisibility"]);

   end DocumentSymbol_Scope;

   procedure Read_DocumentSymbol
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol) is
      use DocumentSymbol_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentSymbol_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  detail
                  Value.detail.Clear;
                  Value.detail.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  kind
                  Read_SymbolKind (Handler, Value.kind);
               when 4 =>  --  tags
                  Read_SymbolTag_Set (Handler, Value.tags);
               when 5 =>  --  deprecated
                  Value.deprecated       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.deprecated.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 7 =>  --  selectionRange
                  Read_A_Range (Handler, Value.selectionRange);
               when 8 =>  --  children
                  Read_DocumentSymbol_Vector (Handler, Value.children);
               when 9 =>  --  alsIsDeclaration
                  Value.alsIsDeclaration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.alsIsDeclaration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 10 =>  --  alsIsAdaProcedure
                  Value.alsIsAdaProcedure       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.alsIsAdaProcedure.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 11 =>  --  alsVisibility
                  Value.alsVisibility :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_AlsVisibility (Handler, Value.alsVisibility.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentSymbol;

   procedure Read_DocumentHighlight_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlight_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DocumentHighlight_Vector renames Value;
         Value : LSP.Structures.DocumentHighlight;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_DocumentHighlight (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_DocumentHighlight_Vector;

   package SignatureHelpParams_Scope is
      package SignatureHelpParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "context"]);

   end SignatureHelpParams_Scope;

   procedure Read_SignatureHelpParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpParams) is
      use SignatureHelpParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureHelpParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 4 =>  --  context
                  Value.context :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SignatureHelpContext (Handler, Value.context.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureHelpParams;

   package Position_Scope is
      package Position_Map is new Minimal_Perfect_Hash
        (["line",
         "character"]);

   end Position_Scope;

   procedure Read_Position
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Position) is
      use Position_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Position_Map.Get_Index (Key) is
               when 1 =>  --  line
                  Value.line := Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  character
                  Value.character :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Position;

   package ExecuteCommandRegistrationOptions_Scope is
      package ExecuteCommandRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "commands"]);

   end ExecuteCommandRegistrationOptions_Scope;

   procedure Read_ExecuteCommandRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandRegistrationOptions) is
      use ExecuteCommandRegistrationOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ExecuteCommandRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  commands
                  Read_Virtual_String_Vector (Handler, Value.commands);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ExecuteCommandRegistrationOptions;

   procedure Read_LSPErrorCodes
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.LSPErrorCodes) is
   begin
      Value :=
        LSP.Enumerations.LSPErrorCodes (Handler.Number_Value.Integer_Value);
      Handler.Read_Next;
   end Read_LSPErrorCodes;

end LSP.Inputs.Part_13;
