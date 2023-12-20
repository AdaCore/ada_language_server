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

package body LSP.Inputs.Part_9 is

   package DidChangeWatchedFilesClientCapabilities_Scope is
      package DidChangeWatchedFilesClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "relativePatternSupport"]);

   end DidChangeWatchedFilesClientCapabilities_Scope;

   procedure Read_DidChangeWatchedFilesClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeWatchedFilesClientCapabilities) is
      use DidChangeWatchedFilesClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeWatchedFilesClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  relativePatternSupport
                  Value.relativePatternSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.relativePatternSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeWatchedFilesClientCapabilities;

   package TextDocumentClientCapabilities_Scope is
      package TextDocumentClientCapabilities_Map is new Minimal_Perfect_Hash
        (["synchronization",
         "completion",
         "hover",
         "signatureHelp",
         "declaration",
         "definition",
         "typeDefinition",
         "implementation",
         "references",
         "documentHighlight",
         "documentSymbol",
         "codeAction",
         "codeLens",
         "documentLink",
         "colorProvider",
         "formatting",
         "rangeFormatting",
         "onTypeFormatting",
         "rename",
         "foldingRange",
         "selectionRange",
         "publishDiagnostics",
         "callHierarchy",
         "semanticTokens",
         "linkedEditingRange",
         "moniker",
         "typeHierarchy",
         "inlineValue",
         "inlayHint",
         "diagnostic"]);

   end TextDocumentClientCapabilities_Scope;

   procedure Read_TextDocumentClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentClientCapabilities) is
      use TextDocumentClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  synchronization
                  Value.synchronization :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TextDocumentSyncClientCapabilities
                    (Handler, Value.synchronization.Value);
               when 2 =>  --  completion
                  Value.completion :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CompletionClientCapabilities
                    (Handler, Value.completion.Value);
               when 3 =>  --  hover
                  Value.hover :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_HoverClientCapabilities (Handler, Value.hover.Value);
               when 4 =>  --  signatureHelp
                  Value.signatureHelp :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SignatureHelpClientCapabilities
                    (Handler, Value.signatureHelp.Value);
               when 5 =>  --  declaration
                  Value.declaration :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DeclarationClientCapabilities
                    (Handler, Value.declaration.Value);
               when 6 =>  --  definition
                  Value.definition :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DefinitionClientCapabilities
                    (Handler, Value.definition.Value);
               when 7 =>  --  typeDefinition
                  Value.typeDefinition :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TypeDefinitionClientCapabilities
                    (Handler, Value.typeDefinition.Value);
               when 8 =>  --  implementation
                  Value.implementation :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ImplementationClientCapabilities
                    (Handler, Value.implementation.Value);
               when 9 =>  --  references
                  Value.references :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ReferenceClientCapabilities
                    (Handler, Value.references.Value);
               when 10 =>  --  documentHighlight
                  Value.documentHighlight :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentHighlightClientCapabilities
                    (Handler, Value.documentHighlight.Value);
               when 11 =>  --  documentSymbol
                  Value.documentSymbol :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentSymbolClientCapabilities
                    (Handler, Value.documentSymbol.Value);
               when 12 =>  --  codeAction
                  Value.codeAction :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeActionClientCapabilities
                    (Handler, Value.codeAction.Value);
               when 13 =>  --  codeLens
                  Value.codeLens :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeLensClientCapabilities
                    (Handler, Value.codeLens.Value);
               when 14 =>  --  documentLink
                  Value.documentLink :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentLinkClientCapabilities
                    (Handler, Value.documentLink.Value);
               when 15 =>  --  colorProvider
                  Value.colorProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentColorClientCapabilities
                    (Handler, Value.colorProvider.Value);
               when 16 =>  --  formatting
                  Value.formatting :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentFormattingClientCapabilities
                    (Handler, Value.formatting.Value);
               when 17 =>  --  rangeFormatting
                  Value.rangeFormatting :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentRangeFormattingClientCapabilities
                    (Handler, Value.rangeFormatting.Value);
               when 18 =>  --  onTypeFormatting
                  Value.onTypeFormatting :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentOnTypeFormattingClientCapabilities
                    (Handler, Value.onTypeFormatting.Value);
               when 19 =>  --  rename
                  Value.rename :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_RenameClientCapabilities (Handler, Value.rename.Value);
               when 20 =>  --  foldingRange
                  Value.foldingRange :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FoldingRangeClientCapabilities
                    (Handler, Value.foldingRange.Value);
               when 21 =>  --  selectionRange
                  Value.selectionRange :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SelectionRangeClientCapabilities
                    (Handler, Value.selectionRange.Value);
               when 22 =>  --  publishDiagnostics
                  Value.publishDiagnostics :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_PublishDiagnosticsClientCapabilities
                    (Handler, Value.publishDiagnostics.Value);
               when 23 =>  --  callHierarchy
                  Value.callHierarchy :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CallHierarchyClientCapabilities
                    (Handler, Value.callHierarchy.Value);
               when 24 =>  --  semanticTokens
                  Value.semanticTokens :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SemanticTokensClientCapabilities
                    (Handler, Value.semanticTokens.Value);
               when 25 =>  --  linkedEditingRange
                  Value.linkedEditingRange :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_LinkedEditingRangeClientCapabilities
                    (Handler, Value.linkedEditingRange.Value);
               when 26 =>  --  moniker
                  Value.moniker :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_MonikerClientCapabilities
                    (Handler, Value.moniker.Value);
               when 27 =>  --  typeHierarchy
                  Value.typeHierarchy :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TypeHierarchyClientCapabilities
                    (Handler, Value.typeHierarchy.Value);
               when 28 =>  --  inlineValue
                  Value.inlineValue :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InlineValueClientCapabilities
                    (Handler, Value.inlineValue.Value);
               when 29 =>  --  inlayHint
                  Value.inlayHint :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InlayHintClientCapabilities
                    (Handler, Value.inlayHint.Value);
               when 30 =>  --  diagnostic
                  Value.diagnostic :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DiagnosticClientCapabilities
                    (Handler, Value.diagnostic.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentClientCapabilities;

   package LinkedEditingRangeClientCapabilities_Scope is
      package LinkedEditingRangeClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end LinkedEditingRangeClientCapabilities_Scope;

   procedure Read_LinkedEditingRangeClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeClientCapabilities) is
      use LinkedEditingRangeClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LinkedEditingRangeClientCapabilities_Map.Get_Index (Key) is
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
   end Read_LinkedEditingRangeClientCapabilities;

   package InlineValueClientCapabilities_Scope is
      package InlineValueClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end InlineValueClientCapabilities_Scope;

   procedure Read_InlineValueClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueClientCapabilities) is
      use InlineValueClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueClientCapabilities_Map.Get_Index (Key) is
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
   end Read_InlineValueClientCapabilities;

   package NotebookCellTextDocumentFilter_Scope is
      package NotebookCellTextDocumentFilter_Map is new Minimal_Perfect_Hash
        (["notebook",
         "language"]);

   end NotebookCellTextDocumentFilter_Scope;

   procedure Read_NotebookCellTextDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCellTextDocumentFilter) is
      use NotebookCellTextDocumentFilter_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookCellTextDocumentFilter_Map.Get_Index (Key) is
               when 1 =>  --  notebook
                  Read_Virtual_String_Or_NotebookDocumentFilter
                    (Handler, Value.notebook);
               when 2 =>  --  language
                  Value.language.Clear;
                  Value.language.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookCellTextDocumentFilter;

   package FileSystemWatcher_Scope is
      package FileSystemWatcher_Map is new Minimal_Perfect_Hash
        (["globPattern",
         "kind"]);

   end FileSystemWatcher_Scope;

   procedure Read_FileSystemWatcher
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileSystemWatcher) is
      use FileSystemWatcher_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileSystemWatcher_Map.Get_Index (Key) is
               when 1 =>  --  globPattern
                  Read_GlobPattern (Handler, Value.globPattern);
               when 2 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WatchKind (Handler, Value.kind.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileSystemWatcher;

   package PrepareRenameParams_Scope is
      package PrepareRenameParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken"]);

   end PrepareRenameParams_Scope;

   procedure Read_PrepareRenameParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PrepareRenameParams) is
      use PrepareRenameParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case PrepareRenameParams_Map.Get_Index (Key) is
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
   end Read_PrepareRenameParams;

   package ImplementationClientCapabilities_Scope is
      package ImplementationClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "linkSupport"]);

   end ImplementationClientCapabilities_Scope;

   procedure Read_ImplementationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationClientCapabilities) is
      use ImplementationClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ImplementationClientCapabilities_Map.Get_Index (Key) is
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
   end Read_ImplementationClientCapabilities;

   package LocationLink_Scope is
      package LocationLink_Map is new Minimal_Perfect_Hash
        (["originSelectionRange",
         "targetUri",
         "targetRange",
         "targetSelectionRange"]);

   end LocationLink_Scope;

   procedure Read_LocationLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LocationLink) is
      use LocationLink_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LocationLink_Map.Get_Index (Key) is
               when 1 =>  --  originSelectionRange
                  Value.originSelectionRange :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_A_Range (Handler, Value.originSelectionRange.Value);
               when 2 =>  --  targetUri
                  Value.targetUri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 3 =>  --  targetRange
                  Read_A_Range (Handler, Value.targetRange);
               when 4 =>  --  targetSelectionRange
                  Read_A_Range (Handler, Value.targetSelectionRange);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_LocationLink;

   package Location_Scope is
      package Location_Map is new Minimal_Perfect_Hash
        (["uri",
         "range",
         "alsKind"]);

   end Location_Scope;

   procedure Read_Location
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Location) is
      use Location_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Location_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 3 =>  --  alsKind
                  Read_AlsReferenceKind_Set (Handler, Value.alsKind);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Location;

   package TypeDefinitionOptions_Scope is
      package TypeDefinitionOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end TypeDefinitionOptions_Scope;

   procedure Read_TypeDefinitionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionOptions) is
      use TypeDefinitionOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeDefinitionOptions_Map.Get_Index (Key) is
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
   end Read_TypeDefinitionOptions;

   procedure Read_TextDocumentSyncKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TextDocumentSyncKind) is
   begin
      Value :=
        LSP.Enumerations.TextDocumentSyncKind'Val
          (Handler.Number_Value.Integer_Value + 0);
      Handler.Read_Next;
   end Read_TextDocumentSyncKind;

   package DocumentColorClientCapabilities_Scope is
      package DocumentColorClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end DocumentColorClientCapabilities_Scope;

   procedure Read_DocumentColorClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorClientCapabilities) is
      use DocumentColorClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentColorClientCapabilities_Map.Get_Index (Key) is
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
   end Read_DocumentColorClientCapabilities;

end LSP.Inputs.Part_9;
