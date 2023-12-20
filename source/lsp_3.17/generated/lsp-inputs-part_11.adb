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

package body LSP.Inputs.Part_11 is

   procedure Read_DocumentLink_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLink_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_DocumentLink_Vector (Handler, Value);
      end if;
   end Read_DocumentLink_Vector_Or_Null;

   procedure Read_Command_Or_CodeAction_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command_Or_CodeAction_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Command_Or_CodeAction_Vector renames Value;
         Value : LSP.Structures.Command_Or_CodeAction;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_Command_Or_CodeAction (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Command_Or_CodeAction_Vector;

   procedure Read_Virtual_String_Or_MarkupContent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String_Or_MarkupContent) is
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
            Read_MarkupContent (Handler, Value.MarkupContent);
      end case;
   end Read_Virtual_String_Or_MarkupContent;

   package FoldingRangeRegistrationOptions_Scope is
      package FoldingRangeRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end FoldingRangeRegistrationOptions_Scope;

   procedure Read_FoldingRangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeRegistrationOptions) is
      use FoldingRangeRegistrationOptions_Scope;
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
            case FoldingRangeRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_FoldingRangeRegistrationOptions;

   package WorkspaceSymbolParams_Scope is
      package WorkspaceSymbolParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "query",
         "case_sensitive",
         "whole_word",
         "negate",
         "kind"]);

   end WorkspaceSymbolParams_Scope;

   procedure Read_WorkspaceSymbolParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolParams) is
      use WorkspaceSymbolParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceSymbolParams_Map.Get_Index (Key) is
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
               when 3 =>  --  query
                  Value.query.Clear;
                  Value.query.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 4 =>  --  case_sensitive
                  Value.case_sensitive       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.case_sensitive.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  whole_word
                  Value.whole_word       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.whole_word.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  negate
                  Value.negate       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.negate.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 7 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_AlsSearchKind (Handler, Value.kind.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceSymbolParams;

   procedure Read_DocumentSymbol_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DocumentSymbol_Vector renames Value;
         Value : LSP.Structures.DocumentSymbol;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_DocumentSymbol (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_DocumentSymbol_Vector;

   package DeleteFileOptions_Scope is
      package DeleteFileOptions_Map is new Minimal_Perfect_Hash
        (["recursive",
         "ignoreIfNotExists"]);

   end DeleteFileOptions_Scope;

   procedure Read_DeleteFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeleteFileOptions) is
      use DeleteFileOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeleteFileOptions_Map.Get_Index (Key) is
               when 1 =>  --  recursive
                  Value.recursive       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.recursive.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  ignoreIfNotExists
                  Value.ignoreIfNotExists       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.ignoreIfNotExists.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DeleteFileOptions;

   procedure Read_LSPAny_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPAny_Vector) is
   begin
      LSP.Input_Tools.Read_LSPAny_Class (Handler, Value);
   end Read_LSPAny_Vector;

   package DeleteFile_Scope is
      package DeleteFile_Map is new Minimal_Perfect_Hash
        (["kind",
         "annotationId",
         "uri",
         "options"]);

   end DeleteFile_Scope;

   procedure Read_DeleteFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeleteFile) is
      use DeleteFile_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeleteFile_Map.Get_Index (Key) is
               when 2 =>  --  annotationId
                  Value.annotationId :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ChangeAnnotationIdentifier
                    (Handler, Value.annotationId.Value);
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: delete
               when 3 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 4 =>  --  options
                  Value.options :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DeleteFileOptions (Handler, Value.options.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DeleteFile;

   package FileDelete_Scope is
      package FileDelete_Map is new Minimal_Perfect_Hash (["uri"]);

   end FileDelete_Scope;

   procedure Read_FileDelete
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileDelete) is
      use FileDelete_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileDelete_Map.Get_Index (Key) is
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
   end Read_FileDelete;

   package DocumentFilter_Scope is
      package DocumentFilter_Map is new Minimal_Perfect_Hash (["notebook"]);

   end DocumentFilter_Scope;

   procedure Read_DocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFilter) is
      use DocumentFilter_Scope;
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
            Value :=
              (Is_TextDocumentFilter => True,
               others                => <>);
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    DocumentFilter_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  notebook
                        Value :=
                          (Is_TextDocumentFilter => False,
                           others                => <>);
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

         case Value.Is_TextDocumentFilter is
            when True =>
               Read_TextDocumentFilter (Handler, Value.TextDocumentFilter);
            when False =>
               Read_NotebookCellTextDocumentFilter
                 (Handler, Value.NotebookCellTextDocumentFilter);
         end case;
      end;
   end Read_DocumentFilter;

   package DocumentHighlight_Scope is
      package DocumentHighlight_Map is new Minimal_Perfect_Hash
        (["range",
         "kind"]);

   end DocumentHighlight_Scope;

   procedure Read_DocumentHighlight
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlight) is
      use DocumentHighlight_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentHighlight_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentHighlightKind (Handler, Value.kind.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentHighlight;

   package DeclarationClientCapabilities_Scope is
      package DeclarationClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "linkSupport"]);

   end DeclarationClientCapabilities_Scope;

   procedure Read_DeclarationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationClientCapabilities) is
      use DeclarationClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeclarationClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  linkSupport
                  Value.linkSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.linkSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DeclarationClientCapabilities;

   package NotebookCell_Scope is
      package NotebookCell_Map is new Minimal_Perfect_Hash
        (["kind",
         "document",
         "metadata",
         "executionSummary"]);

   end NotebookCell_Scope;

   procedure Read_NotebookCell
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCell) is
      use NotebookCell_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookCell_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Read_NotebookCellKind (Handler, Value.kind);
               when 2 =>  --  document
                  Value.document := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 3 =>  --  metadata
                  Value.metadata :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_LSPObject (Handler, Value.metadata.Value);
               when 4 =>  --  executionSummary
                  Value.executionSummary :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ExecutionSummary
                    (Handler, Value.executionSummary.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookCell;

   package DocumentSymbol_Result_Scope is
      package DocumentSymbol_Result_Map is new Minimal_Perfect_Hash
        (["location",
         "containerName",
         "detail",
         "range",
         "selectionRange",
         "children",
         "alsIsDeclaration",
         "alsIsAdaProcedure",
         "alsVisibility"]);

   end DocumentSymbol_Result_Scope;

   procedure Read_DocumentSymbol_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol_Result) is
      use DocumentSymbol_Result_Scope;
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
                    DocumentSymbol_Result_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  location
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  containerName
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 3 =>  --  detail
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 4 =>  --  range
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 5 =>  --  selectionRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 6 =>  --  children
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 7 =>  --  alsIsDeclaration
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 8 =>  --  alsIsAdaProcedure
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 9 =>  --  alsVisibility
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
               Read_DocumentSymbol_Vector (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               null;  --  #null_value
               Handler.Read_Next;
         end case;
      end;
   end Read_DocumentSymbol_Result;

   procedure Read_Moniker_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Moniker_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Moniker_Vector renames Value;
         Value : LSP.Structures.Moniker;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_Moniker (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Moniker_Vector;

   package DocumentHighlightOptions_Scope is
      package DocumentHighlightOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end DocumentHighlightOptions_Scope;

   procedure Read_DocumentHighlightOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightOptions) is
      use DocumentHighlightOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentHighlightOptions_Map.Get_Index (Key) is
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
   end Read_DocumentHighlightOptions;

   procedure Read_SelectionRange_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRange_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_SelectionRange_Vector (Handler, Value);
      end if;
   end Read_SelectionRange_Vector_Or_Null;

end LSP.Inputs.Part_11;
