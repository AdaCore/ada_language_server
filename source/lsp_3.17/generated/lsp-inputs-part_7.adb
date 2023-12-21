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

package body LSP.Inputs.Part_7 is

   package TextDocumentRegistrationOptions_Scope is
      package TextDocumentRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector"]);

   end TextDocumentRegistrationOptions_Scope;

   procedure Read_TextDocumentRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentRegistrationOptions) is
      use TextDocumentRegistrationOptions_Scope;
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
            case TextDocumentRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.documentSelector);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentRegistrationOptions;

   package ServerCapabilities_Scope is
      package ServerCapabilities_Map is new Minimal_Perfect_Hash
        (["positionEncoding",
         "textDocumentSync",
         "notebookDocumentSync",
         "completionProvider",
         "hoverProvider",
         "signatureHelpProvider",
         "declarationProvider",
         "definitionProvider",
         "typeDefinitionProvider",
         "implementationProvider",
         "referencesProvider",
         "documentHighlightProvider",
         "documentSymbolProvider",
         "codeActionProvider",
         "codeLensProvider",
         "documentLinkProvider",
         "colorProvider",
         "workspaceSymbolProvider",
         "documentFormattingProvider",
         "documentRangeFormattingProvider",
         "documentOnTypeFormattingProvider",
         "renameProvider",
         "foldingRangeProvider",
         "selectionRangeProvider",
         "executeCommandProvider",
         "callHierarchyProvider",
         "linkedEditingRangeProvider",
         "semanticTokensProvider",
         "monikerProvider",
         "typeHierarchyProvider",
         "inlineValueProvider",
         "inlayHintProvider",
         "diagnosticProvider",
         "workspace",
         "experimental",
         "alsReferenceKinds"]);

      package NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions_Scope
      is
         package NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions_Map is new Minimal_Perfect_Hash
           (["id"]);

      end NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions_Scope;

      package implementationProvider_OfServerCapabilities_Scope is
         package implementationProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end implementationProvider_OfServerCapabilities_Scope;

      package typeHierarchyProvider_OfServerCapabilities_Scope is
         package typeHierarchyProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end typeHierarchyProvider_OfServerCapabilities_Scope;

      package DiagnosticOptions_Or_DiagnosticRegistrationOptions_Scope is
         package DiagnosticOptions_Or_DiagnosticRegistrationOptions_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end DiagnosticOptions_Or_DiagnosticRegistrationOptions_Scope;

      package inlayHintProvider_OfServerCapabilities_Scope is
         package inlayHintProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end inlayHintProvider_OfServerCapabilities_Scope;

      package SemanticTokensOptions_Or_SemanticTokensRegistrationOptions_Scope
      is
         package SemanticTokensOptions_Or_SemanticTokensRegistrationOptions_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end SemanticTokensOptions_Or_SemanticTokensRegistrationOptions_Scope;

      package monikerProvider_OfServerCapabilities_Scope is
         package monikerProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector"]);

      end monikerProvider_OfServerCapabilities_Scope;

      package selectionRangeProvider_OfServerCapabilities_Scope is
         package selectionRangeProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end selectionRangeProvider_OfServerCapabilities_Scope;

      package workspace_OfServerCapabilities_Scope is
         package workspace_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["workspaceFolders",
            "fileOperations"]);

      end workspace_OfServerCapabilities_Scope;

      package declarationProvider_OfServerCapabilities_Scope is
         package declarationProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end declarationProvider_OfServerCapabilities_Scope;

      package callHierarchyProvider_OfServerCapabilities_Scope is
         package callHierarchyProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end callHierarchyProvider_OfServerCapabilities_Scope;

      package linkedEditingRangeProvider_OfServerCapabilities_Scope is
         package linkedEditingRangeProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end linkedEditingRangeProvider_OfServerCapabilities_Scope;

      package foldingRangeProvider_OfServerCapabilities_Scope is
         package foldingRangeProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end foldingRangeProvider_OfServerCapabilities_Scope;

      package typeDefinitionProvider_OfServerCapabilities_Scope is
         package typeDefinitionProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end typeDefinitionProvider_OfServerCapabilities_Scope;

      package inlineValueProvider_OfServerCapabilities_Scope is
         package inlineValueProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end inlineValueProvider_OfServerCapabilities_Scope;

      package colorProvider_OfServerCapabilities_Scope is
         package colorProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end colorProvider_OfServerCapabilities_Scope;

   end ServerCapabilities_Scope;

   procedure Read_ServerCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ServerCapabilities) is
      use ServerCapabilities_Scope;
      procedure Read_Boolean_Or_DocumentSymbolOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DocumentSymbolOptions);

      procedure Read_NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions);

      procedure Read_Boolean_Or_WorkspaceSymbolOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_WorkspaceSymbolOptions);

      procedure Read_implementationProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .implementationProvider_OfServerCapabilities);

      procedure Read_typeHierarchyProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .typeHierarchyProvider_OfServerCapabilities);

      procedure Read_DiagnosticOptions_Or_DiagnosticRegistrationOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .DiagnosticOptions_Or_DiagnosticRegistrationOptions);

      procedure Read_inlayHintProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.inlayHintProvider_OfServerCapabilities);

      procedure Read_SemanticTokensOptions_Or_SemanticTokensRegistrationOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .SemanticTokensOptions_Or_SemanticTokensRegistrationOptions);

      procedure Read_monikerProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.monikerProvider_OfServerCapabilities);

      procedure Read_Boolean_Or_CodeActionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_CodeActionOptions);

      procedure Read_selectionRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .selectionRangeProvider_OfServerCapabilities);

      procedure Read_Boolean_Or_DocumentRangeFormattingOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value : out LSP.Structures.Boolean_Or_DocumentRangeFormattingOptions);

      procedure Read_Boolean_Or_HoverOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_HoverOptions);

      procedure Read_workspace_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.workspace_OfServerCapabilities);

      procedure Read_declarationProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value : out LSP.Structures.declarationProvider_OfServerCapabilities);

      procedure Read_callHierarchyProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .callHierarchyProvider_OfServerCapabilities);

      procedure Read_Boolean_Or_DocumentHighlightOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DocumentHighlightOptions);

      procedure Read_TextDocumentSyncOptions_Or_TextDocumentSyncKind
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .TextDocumentSyncOptions_Or_TextDocumentSyncKind);

      procedure Read_linkedEditingRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .linkedEditingRangeProvider_OfServerCapabilities);

      procedure Read_Boolean_Or_DocumentFormattingOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DocumentFormattingOptions);

      procedure Read_Boolean_Or_ReferenceOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_ReferenceOptions);

      procedure Read_foldingRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value : out LSP.Structures.foldingRangeProvider_OfServerCapabilities);

      procedure Read_typeDefinitionProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .typeDefinitionProvider_OfServerCapabilities);

      procedure Read_inlineValueProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value : out LSP.Structures.inlineValueProvider_OfServerCapabilities);

      procedure Read_colorProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.colorProvider_OfServerCapabilities);

      procedure Read_Boolean_Or_DefinitionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DefinitionOptions);

      procedure Read_Boolean_Or_RenameOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_RenameOptions);

      procedure Read_Boolean_Or_DocumentSymbolOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DocumentSymbolOptions) is
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
               Read_DocumentSymbolOptions
                 (Handler, Value.DocumentSymbolOptions);
         end case;
      end Read_Boolean_Or_DocumentSymbolOptions;

      procedure Read_NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions) is
         use
           NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions_Scope;
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
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Is_NotebookDocumentSyncOptions => True,
                  others                         => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  id
                           Value :=
                             (Is_NotebookDocumentSyncOptions => False,
                              others                         => <>);
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

            case Value.Is_NotebookDocumentSyncOptions is
               when True =>
                  Read_NotebookDocumentSyncOptions
                    (Handler, Value.NotebookDocumentSyncOptions);
               when False =>
                  Read_NotebookDocumentSyncRegistrationOptions
                    (Handler, Value.NotebookDocumentSyncRegistrationOptions);
            end case;
         end;
      end Read_NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions;

      procedure Read_Boolean_Or_WorkspaceSymbolOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_WorkspaceSymbolOptions) is
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
               Read_WorkspaceSymbolOptions
                 (Handler, Value.WorkspaceSymbolOptions);
         end case;
      end Read_Boolean_Or_WorkspaceSymbolOptions;

      procedure Read_implementationProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .implementationProvider_OfServerCapabilities) is
         use implementationProvider_OfServerCapabilities_Scope;
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
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       implementationProvider_OfServerCapabilities_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
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
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_ImplementationOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_ImplementationRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_implementationProvider_OfServerCapabilities;

      procedure Read_typeHierarchyProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .typeHierarchyProvider_OfServerCapabilities) is
         use typeHierarchyProvider_OfServerCapabilities_Scope;
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
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       typeHierarchyProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
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
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_TypeHierarchyOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_TypeHierarchyRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_typeHierarchyProvider_OfServerCapabilities;

      procedure Read_DiagnosticOptions_Or_DiagnosticRegistrationOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .DiagnosticOptions_Or_DiagnosticRegistrationOptions) is
         use DiagnosticOptions_Or_DiagnosticRegistrationOptions_Scope;
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
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Is_DiagnosticOptions => True,
                  others               => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       DiagnosticOptions_Or_DiagnosticRegistrationOptions_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Is_DiagnosticOptions => False,
                              others               => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Is_DiagnosticOptions => False,
                              others               => <>);
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

            case Value.Is_DiagnosticOptions is
               when True =>
                  Read_DiagnosticOptions (Handler, Value.DiagnosticOptions);
               when False =>
                  Read_DiagnosticRegistrationOptions
                    (Handler, Value.DiagnosticRegistrationOptions);
            end case;
         end;
      end Read_DiagnosticOptions_Or_DiagnosticRegistrationOptions;

      procedure Read_inlayHintProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value : out LSP.Structures.inlayHintProvider_OfServerCapabilities) is
         use inlayHintProvider_OfServerCapabilities_Scope;
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
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       inlayHintProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
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
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_InlayHintOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_InlayHintRegistrationOptions (Handler, Value.Variant_3);
            end case;
         end;
      end Read_inlayHintProvider_OfServerCapabilities;

      procedure Read_SemanticTokensOptions_Or_SemanticTokensRegistrationOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .SemanticTokensOptions_Or_SemanticTokensRegistrationOptions) is
         use SemanticTokensOptions_Or_SemanticTokensRegistrationOptions_Scope;
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
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Is_SemanticTokensOptions => True,
                  others                   => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       SemanticTokensOptions_Or_SemanticTokensRegistrationOptions_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Is_SemanticTokensOptions => False,
                              others                   => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Is_SemanticTokensOptions => False,
                              others                   => <>);
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

            case Value.Is_SemanticTokensOptions is
               when True =>
                  Read_SemanticTokensOptions
                    (Handler, Value.SemanticTokensOptions);
               when False =>
                  Read_SemanticTokensRegistrationOptions
                    (Handler, Value.SemanticTokensRegistrationOptions);
            end case;
         end;
      end Read_SemanticTokensOptions_Or_SemanticTokensRegistrationOptions;

      procedure Read_monikerProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.monikerProvider_OfServerCapabilities) is
         use monikerProvider_OfServerCapabilities_Scope;
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
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       monikerProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
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
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_MonikerOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_MonikerRegistrationOptions (Handler, Value.Variant_3);
            end case;
         end;
      end Read_monikerProvider_OfServerCapabilities;

      procedure Read_Boolean_Or_CodeActionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_CodeActionOptions) is
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
               Read_CodeActionOptions (Handler, Value.CodeActionOptions);
         end case;
      end Read_Boolean_Or_CodeActionOptions;

      procedure Read_selectionRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .selectionRangeProvider_OfServerCapabilities) is
         use selectionRangeProvider_OfServerCapabilities_Scope;
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
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       selectionRangeProvider_OfServerCapabilities_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
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
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_SelectionRangeOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_SelectionRangeRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_selectionRangeProvider_OfServerCapabilities;

      procedure Read_Boolean_Or_DocumentRangeFormattingOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .Boolean_Or_DocumentRangeFormattingOptions) is
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
               Read_DocumentRangeFormattingOptions
                 (Handler, Value.DocumentRangeFormattingOptions);
         end case;
      end Read_Boolean_Or_DocumentRangeFormattingOptions;

      procedure Read_Boolean_Or_HoverOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_HoverOptions) is
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
               Read_HoverOptions (Handler, Value.HoverOptions);
         end case;
      end Read_Boolean_Or_HoverOptions;

      procedure Read_workspace_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.workspace_OfServerCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use workspace_OfServerCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case workspace_OfServerCapabilities_Map.Get_Index (Key) is
                  when 1 =>  --  workspaceFolders
                     Value.workspaceFolders :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_WorkspaceFoldersServerCapabilities
                       (Handler, Value.workspaceFolders.Value);
                  when 2 =>  --  fileOperations
                     Value.fileOperations :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_FileOperationOptions
                       (Handler, Value.fileOperations.Value);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_workspace_OfServerCapabilities;

      procedure Read_declarationProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .declarationProvider_OfServerCapabilities) is
         use declarationProvider_OfServerCapabilities_Scope;
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
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       declarationProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
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
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_DeclarationOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_DeclarationRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_declarationProvider_OfServerCapabilities;

      procedure Read_callHierarchyProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .callHierarchyProvider_OfServerCapabilities) is
         use callHierarchyProvider_OfServerCapabilities_Scope;
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
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       callHierarchyProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
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
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_CallHierarchyOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_CallHierarchyRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_callHierarchyProvider_OfServerCapabilities;

      procedure Read_Boolean_Or_DocumentHighlightOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DocumentHighlightOptions) is
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
               Read_DocumentHighlightOptions
                 (Handler, Value.DocumentHighlightOptions);
         end case;
      end Read_Boolean_Or_DocumentHighlightOptions;

      procedure Read_TextDocumentSyncOptions_Or_TextDocumentSyncKind
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .TextDocumentSyncOptions_Or_TextDocumentSyncKind) is
      begin
         if Handler.Is_Number_Value then
            Value :=
              (Is_TextDocumentSyncOptions => False,
               others                     => <>);
         else
            Value :=
              (Is_TextDocumentSyncOptions => True,
               others                     => <>);
         end if;

         case Value.Is_TextDocumentSyncOptions is
            when True =>
               Read_TextDocumentSyncOptions
                 (Handler, Value.TextDocumentSyncOptions);
            when False =>
               Read_TextDocumentSyncKind (Handler, Value.TextDocumentSyncKind);
         end case;
      end Read_TextDocumentSyncOptions_Or_TextDocumentSyncKind;

      procedure Read_linkedEditingRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .linkedEditingRangeProvider_OfServerCapabilities) is
         use linkedEditingRangeProvider_OfServerCapabilities_Scope;
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
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       linkedEditingRangeProvider_OfServerCapabilities_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
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
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_LinkedEditingRangeOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_LinkedEditingRangeRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_linkedEditingRangeProvider_OfServerCapabilities;

      procedure Read_Boolean_Or_DocumentFormattingOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DocumentFormattingOptions) is
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
               Read_DocumentFormattingOptions
                 (Handler, Value.DocumentFormattingOptions);
         end case;
      end Read_Boolean_Or_DocumentFormattingOptions;

      procedure Read_Boolean_Or_ReferenceOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_ReferenceOptions) is
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
               Read_ReferenceOptions (Handler, Value.ReferenceOptions);
         end case;
      end Read_Boolean_Or_ReferenceOptions;

      procedure Read_foldingRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .foldingRangeProvider_OfServerCapabilities) is
         use foldingRangeProvider_OfServerCapabilities_Scope;
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
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       foldingRangeProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
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
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_FoldingRangeOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_FoldingRangeRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_foldingRangeProvider_OfServerCapabilities;

      procedure Read_typeDefinitionProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .typeDefinitionProvider_OfServerCapabilities) is
         use typeDefinitionProvider_OfServerCapabilities_Scope;
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
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       typeDefinitionProvider_OfServerCapabilities_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
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
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_TypeDefinitionOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_TypeDefinitionRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_typeDefinitionProvider_OfServerCapabilities;

      procedure Read_inlineValueProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .inlineValueProvider_OfServerCapabilities) is
         use inlineValueProvider_OfServerCapabilities_Scope;
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
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       inlineValueProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
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
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_InlineValueOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_InlineValueRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_inlineValueProvider_OfServerCapabilities;

      procedure Read_colorProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.colorProvider_OfServerCapabilities) is
         use colorProvider_OfServerCapabilities_Scope;
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
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       colorProvider_OfServerCapabilities_Map.Get_Index (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
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
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_DocumentColorOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_DocumentColorRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_colorProvider_OfServerCapabilities;

      procedure Read_Boolean_Or_DefinitionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DefinitionOptions) is
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
               Read_DefinitionOptions (Handler, Value.DefinitionOptions);
         end case;
      end Read_Boolean_Or_DefinitionOptions;

      procedure Read_Boolean_Or_RenameOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_RenameOptions) is
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
               Read_RenameOptions (Handler, Value.RenameOptions);
         end case;
      end Read_Boolean_Or_RenameOptions;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ServerCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  positionEncoding
                  Value.positionEncoding :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_PositionEncodingKind
                    (Handler, Value.positionEncoding.Value);
               when 2 =>  --  textDocumentSync
                  Value.textDocumentSync :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TextDocumentSyncOptions_Or_TextDocumentSyncKind
                    (Handler, Value.textDocumentSync.Value);
               when 3 =>  --  notebookDocumentSync
                  Value.notebookDocumentSync :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions
                    (Handler, Value.notebookDocumentSync.Value);
               when 4 =>  --  completionProvider
                  Value.completionProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CompletionOptions
                    (Handler, Value.completionProvider.Value);
               when 5 =>  --  hoverProvider
                  Value.hoverProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_HoverOptions
                    (Handler, Value.hoverProvider.Value);
               when 6 =>  --  signatureHelpProvider
                  Value.signatureHelpProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SignatureHelpOptions
                    (Handler, Value.signatureHelpProvider.Value);
               when 7 =>  --  declarationProvider
                  Value.declarationProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_declarationProvider_OfServerCapabilities
                    (Handler, Value.declarationProvider.Value);
               when 8 =>  --  definitionProvider
                  Value.definitionProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_DefinitionOptions
                    (Handler, Value.definitionProvider.Value);
               when 9 =>  --  typeDefinitionProvider
                  Value.typeDefinitionProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_typeDefinitionProvider_OfServerCapabilities
                    (Handler, Value.typeDefinitionProvider.Value);
               when 10 =>  --  implementationProvider
                  Value.implementationProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_implementationProvider_OfServerCapabilities
                    (Handler, Value.implementationProvider.Value);
               when 11 =>  --  referencesProvider
                  Value.referencesProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_ReferenceOptions
                    (Handler, Value.referencesProvider.Value);
               when 12 =>  --  documentHighlightProvider
                  Value.documentHighlightProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_DocumentHighlightOptions
                    (Handler, Value.documentHighlightProvider.Value);
               when 13 =>  --  documentSymbolProvider
                  Value.documentSymbolProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_DocumentSymbolOptions
                    (Handler, Value.documentSymbolProvider.Value);
               when 14 =>  --  codeActionProvider
                  Value.codeActionProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_CodeActionOptions
                    (Handler, Value.codeActionProvider.Value);
               when 15 =>  --  codeLensProvider
                  Value.codeLensProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeLensOptions (Handler, Value.codeLensProvider.Value);
               when 16 =>  --  documentLinkProvider
                  Value.documentLinkProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentLinkOptions
                    (Handler, Value.documentLinkProvider.Value);
               when 17 =>  --  colorProvider
                  Value.colorProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_colorProvider_OfServerCapabilities
                    (Handler, Value.colorProvider.Value);
               when 18 =>  --  workspaceSymbolProvider
                  Value.workspaceSymbolProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_WorkspaceSymbolOptions
                    (Handler, Value.workspaceSymbolProvider.Value);
               when 19 =>  --  documentFormattingProvider
                  Value.documentFormattingProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_DocumentFormattingOptions
                    (Handler, Value.documentFormattingProvider.Value);
               when 20 =>  --  documentRangeFormattingProvider
                  Value.documentRangeFormattingProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_DocumentRangeFormattingOptions
                    (Handler, Value.documentRangeFormattingProvider.Value);
               when 21 =>  --  documentOnTypeFormattingProvider
                  Value.documentOnTypeFormattingProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentOnTypeFormattingOptions
                    (Handler, Value.documentOnTypeFormattingProvider.Value);
               when 22 =>  --  renameProvider
                  Value.renameProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_RenameOptions
                    (Handler, Value.renameProvider.Value);
               when 23 =>  --  foldingRangeProvider
                  Value.foldingRangeProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_foldingRangeProvider_OfServerCapabilities
                    (Handler, Value.foldingRangeProvider.Value);
               when 24 =>  --  selectionRangeProvider
                  Value.selectionRangeProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_selectionRangeProvider_OfServerCapabilities
                    (Handler, Value.selectionRangeProvider.Value);
               when 25 =>  --  executeCommandProvider
                  Value.executeCommandProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ExecuteCommandOptions
                    (Handler, Value.executeCommandProvider.Value);
               when 26 =>  --  callHierarchyProvider
                  Value.callHierarchyProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_callHierarchyProvider_OfServerCapabilities
                    (Handler, Value.callHierarchyProvider.Value);
               when 27 =>  --  linkedEditingRangeProvider
                  Value.linkedEditingRangeProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_linkedEditingRangeProvider_OfServerCapabilities
                    (Handler, Value.linkedEditingRangeProvider.Value);
               when 28 =>  --  semanticTokensProvider
                  Value.semanticTokensProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SemanticTokensOptions_Or_SemanticTokensRegistrationOptions
                    (Handler, Value.semanticTokensProvider.Value);
               when 29 =>  --  monikerProvider
                  Value.monikerProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_monikerProvider_OfServerCapabilities
                    (Handler, Value.monikerProvider.Value);
               when 30 =>  --  typeHierarchyProvider
                  Value.typeHierarchyProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_typeHierarchyProvider_OfServerCapabilities
                    (Handler, Value.typeHierarchyProvider.Value);
               when 31 =>  --  inlineValueProvider
                  Value.inlineValueProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_inlineValueProvider_OfServerCapabilities
                    (Handler, Value.inlineValueProvider.Value);
               when 32 =>  --  inlayHintProvider
                  Value.inlayHintProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_inlayHintProvider_OfServerCapabilities
                    (Handler, Value.inlayHintProvider.Value);
               when 33 =>  --  diagnosticProvider
                  Value.diagnosticProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DiagnosticOptions_Or_DiagnosticRegistrationOptions
                    (Handler, Value.diagnosticProvider.Value);
               when 34 =>  --  workspace
                  Value.workspace :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_workspace_OfServerCapabilities
                    (Handler, Value.workspace.Value);
               when 35 =>  --  experimental
                  Value.experimental :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_T (Handler, Value.experimental.Value);
               when 36 =>  --  alsReferenceKinds
                  Read_AlsReferenceKind_Set (Handler, Value.alsReferenceKinds);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ServerCapabilities;

   package DidChangeWatchedFilesRegistrationOptions_Scope is
      package DidChangeWatchedFilesRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["watchers"]);

   end DidChangeWatchedFilesRegistrationOptions_Scope;

   procedure Read_DidChangeWatchedFilesRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeWatchedFilesRegistrationOptions) is
      use DidChangeWatchedFilesRegistrationOptions_Scope;
      procedure Read_FileSystemWatcher_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileSystemWatcher_Vector);

      procedure Read_FileSystemWatcher_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileSystemWatcher_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FileSystemWatcher_Vector renames Value;
            Value : LSP.Structures.FileSystemWatcher;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FileSystemWatcher (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FileSystemWatcher_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeWatchedFilesRegistrationOptions_Map.Get_Index
              (Key) is
               when 1 =>  --  watchers
                  Read_FileSystemWatcher_Vector (Handler, Value.watchers);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeWatchedFilesRegistrationOptions;

   package ImplementationRegistrationOptions_Scope is
      package ImplementationRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end ImplementationRegistrationOptions_Scope;

   procedure Read_ImplementationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationRegistrationOptions) is
      use ImplementationRegistrationOptions_Scope;
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
            case ImplementationRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_ImplementationRegistrationOptions;

   procedure Read_WorkspaceFolder_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFolder_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_WorkspaceFolder_Vector (Handler, Value);
      end if;
   end Read_WorkspaceFolder_Vector_Or_Null;

   package DidChangeConfigurationParams_Scope is
      package DidChangeConfigurationParams_Map is new Minimal_Perfect_Hash
        (["settings"]);

   end DidChangeConfigurationParams_Scope;

   procedure Read_DidChangeConfigurationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeConfigurationParams) is
      use DidChangeConfigurationParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeConfigurationParams_Map.Get_Index (Key) is
               when 1 =>  --  settings
                  Read_LSPAny (Handler, Value.settings);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeConfigurationParams;

   package FileOperationOptions_Scope is
      package FileOperationOptions_Map is new Minimal_Perfect_Hash
        (["didCreate",
         "willCreate",
         "didRename",
         "willRename",
         "didDelete",
         "willDelete"]);

   end FileOperationOptions_Scope;

   procedure Read_FileOperationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationOptions) is
      use FileOperationOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileOperationOptions_Map.Get_Index (Key) is
               when 1 =>  --  didCreate
                  Value.didCreate :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationRegistrationOptions
                    (Handler, Value.didCreate.Value);
               when 2 =>  --  willCreate
                  Value.willCreate :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationRegistrationOptions
                    (Handler, Value.willCreate.Value);
               when 3 =>  --  didRename
                  Value.didRename :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationRegistrationOptions
                    (Handler, Value.didRename.Value);
               when 4 =>  --  willRename
                  Value.willRename :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationRegistrationOptions
                    (Handler, Value.willRename.Value);
               when 5 =>  --  didDelete
                  Value.didDelete :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationRegistrationOptions
                    (Handler, Value.didDelete.Value);
               when 6 =>  --  willDelete
                  Value.willDelete :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationRegistrationOptions
                    (Handler, Value.willDelete.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileOperationOptions;

   package CodeLensWorkspaceClientCapabilities_Scope is
      package CodeLensWorkspaceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["refreshSupport"]);

   end CodeLensWorkspaceClientCapabilities_Scope;

   procedure Read_CodeLensWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensWorkspaceClientCapabilities) is
      use CodeLensWorkspaceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeLensWorkspaceClientCapabilities_Map.Get_Index (Key) is
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
   end Read_CodeLensWorkspaceClientCapabilities;

   procedure Read_SymbolKind_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolKind_Set) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.SymbolKind_Set renames Value;
         Value : LSP.Enumerations.SymbolKind;
      begin
         Set := (others => False);
         while not Handler.Is_End_Array loop
            Read_SymbolKind (Handler, Value);
            Set (Value) := True;
         end loop;
      end;

      Handler.Read_Next;
   end Read_SymbolKind_Set;

   package WorkspaceSymbol_Scope is
      package WorkspaceSymbol_Map is new Minimal_Perfect_Hash
        (["name",
         "kind",
         "tags",
         "containerName",
         "location",
         "data"]);

      package Location_Or_Something_Scope is
         package Location_Or_Something_Map is new Minimal_Perfect_Hash
           (["range",
            "alsKind",
            "uri"]);

      end Location_Or_Something_Scope;

   end WorkspaceSymbol_Scope;

   procedure Read_WorkspaceSymbol
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbol) is
      use WorkspaceSymbol_Scope;
      procedure Read_Location_Or_Something
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Location_Or_Something);

      procedure Read_Location_Or_Something
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Location_Or_Something) is
         use Location_Or_Something_Scope;
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
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Is_Location => False,
                  others      => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       Location_Or_Something_Map.Get_Index (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  range
                           Value :=
                             (Is_Location => True,
                              others      => <>);
                           exit;
                        when 2 =>  --  alsKind
                           Value :=
                             (Is_Location => True,
                              others      => <>);
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

            case Value.Is_Location is
               when True =>
                  Read_Location (Handler, Value.Location);
               when False =>
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while Handler.Is_Key_Name loop
                     declare
                        Key : constant VSS.Strings.Virtual_String :=
                          Handler.Key_Name;
                     begin
                        Handler.Read_Next;
                        case Location_Or_Something_Map.Get_Index (Key) is
                           when 3 =>  --  uri
                              Value.uri :=
                                (Handler.String_Value with null record);
                              Handler.Read_Next;
                           when others =>
                              Handler.Skip_Current_Value;
                        end case;
                     end;
                  end loop;

                  Handler.Read_Next;
            end case;
         end;
      end Read_Location_Or_Something;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceSymbol_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  kind
                  Read_SymbolKind (Handler, Value.kind);
               when 3 =>  --  tags
                  Read_SymbolTag_Set (Handler, Value.tags);
               when 4 =>  --  containerName
                  Value.containerName.Clear;
                  Value.containerName.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  location
                  Read_Location_Or_Something (Handler, Value.location);
               when 6 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceSymbol;

   package relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Scope
   is
      package relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Map is new Minimal_Perfect_Hash
        (["full",
         "unchanged"]);

   end relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Scope;

   procedure Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item) is
      use relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Scope;
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
                    relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Map
                      .Get_Index
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
               Read_FullDocumentDiagnosticReport (Handler, Value.full);
            when LSP.Structures.unchanged =>
               Read_UnchangedDocumentDiagnosticReport
                 (Handler, Value.unchanged);
         end case;
      end;
   end Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item;

   package WorkspaceSymbolOptions_Scope is
      package WorkspaceSymbolOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "resolveProvider"]);

   end WorkspaceSymbolOptions_Scope;

   procedure Read_WorkspaceSymbolOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolOptions) is
      use WorkspaceSymbolOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceSymbolOptions_Map.Get_Index (Key) is
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
   end Read_WorkspaceSymbolOptions;

   procedure Read_MessageType
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.MessageType) is
   begin
      Value :=
        LSP.Enumerations.MessageType'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_MessageType;

   package InlineValueRegistrationOptions_Scope is
      package InlineValueRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "documentSelector",
         "id"]);

   end InlineValueRegistrationOptions_Scope;

   procedure Read_InlineValueRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueRegistrationOptions) is
      use InlineValueRegistrationOptions_Scope;
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
            case InlineValueRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_InlineValueRegistrationOptions;

   package DocumentHighlightParams_Scope is
      package DocumentHighlightParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken"]);

   end DocumentHighlightParams_Scope;

   procedure Read_DocumentHighlightParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightParams) is
      use DocumentHighlightParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentHighlightParams_Map.Get_Index (Key) is
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
   end Read_DocumentHighlightParams;

   package WorkspaceDocumentDiagnosticReport_Scope is
      package WorkspaceDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["full",
         "unchanged"]);

   end WorkspaceDocumentDiagnosticReport_Scope;

   procedure Read_WorkspaceDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDocumentDiagnosticReport) is
      use WorkspaceDocumentDiagnosticReport_Scope;
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
                    WorkspaceDocumentDiagnosticReport_Map.Get_Index
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
               Read_WorkspaceFullDocumentDiagnosticReport
                 (Handler, Value.full);
            when LSP.Structures.unchanged =>
               Read_WorkspaceUnchangedDocumentDiagnosticReport
                 (Handler, Value.unchanged);
         end case;
      end;
   end Read_WorkspaceDocumentDiagnosticReport;

   package DocumentColorRegistrationOptions_Scope is
      package DocumentColorRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end DocumentColorRegistrationOptions_Scope;

   procedure Read_DocumentColorRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorRegistrationOptions) is
      use DocumentColorRegistrationOptions_Scope;
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
            case DocumentColorRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_DocumentColorRegistrationOptions;

   package WorkDoneProgressEnd_Scope is
      package WorkDoneProgressEnd_Map is new Minimal_Perfect_Hash
        (["kind",
         "message"]);

   end WorkDoneProgressEnd_Scope;

   procedure Read_WorkDoneProgressEnd
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressEnd) is
      use WorkDoneProgressEnd_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkDoneProgressEnd_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: end
               when 2 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkDoneProgressEnd;

   package FileOperationPatternKind_Map is new Minimal_Perfect_Hash
     (["file",
      "folder"]);

   procedure Read_FileOperationPatternKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FileOperationPatternKind) is
   begin
      Value :=
        LSP.Enumerations.FileOperationPatternKind'Val
          (FileOperationPatternKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_FileOperationPatternKind;

   package LinkedEditingRangeRegistrationOptions_Scope is
      package LinkedEditingRangeRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end LinkedEditingRangeRegistrationOptions_Scope;

   procedure Read_LinkedEditingRangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeRegistrationOptions) is
      use LinkedEditingRangeRegistrationOptions_Scope;
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
            case LinkedEditingRangeRegistrationOptions_Map.Get_Index (Key) is
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
   end Read_LinkedEditingRangeRegistrationOptions;

   package SemanticTokensLegend_Scope is
      package SemanticTokensLegend_Map is new Minimal_Perfect_Hash
        (["tokenTypes",
         "tokenModifiers"]);

   end SemanticTokensLegend_Scope;

   procedure Read_SemanticTokensLegend
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensLegend) is
      use SemanticTokensLegend_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensLegend_Map.Get_Index (Key) is
               when 1 =>  --  tokenTypes
                  Read_Virtual_String_Vector (Handler, Value.tokenTypes);
               when 2 =>  --  tokenModifiers
                  Read_Virtual_String_Vector (Handler, Value.tokenModifiers);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensLegend;

end LSP.Inputs.Part_7;
