--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.String_Vectors;

package LSP.Structures.Unwrap is
   pragma Preelaborate;

   function foldingRange (X : TextDocumentClientCapabilities_Optional)
     return FoldingRangeClientCapabilities_Optional is
       (if X.Is_Set then X.Value.foldingRange else (Is_Set => False));

   function semanticTokens (X : TextDocumentClientCapabilities_Optional)
     return SemanticTokensClientCapabilities_Optional is
       (if X.Is_Set then X.Value.semanticTokens else (Is_Set => False));

   function tokenTypes (X : SemanticTokensClientCapabilities_Optional)
     return LSP.Structures.Virtual_String_Vector is
       (if X.Is_Set then X.Value.tokenTypes
        else VSS.String_Vectors.Empty_Virtual_String_Vector);

   function tokenModifiers (X : SemanticTokensClientCapabilities_Optional)
     return LSP.Structures.Virtual_String_Vector is
       (if X.Is_Set then X.Value.tokenModifiers
        else VSS.String_Vectors.Empty_Virtual_String_Vector);

   function lineFoldingOnly (X : FoldingRangeClientCapabilities_Optional)
     return Boolean_Optional is
       (if X.Is_Set then X.Value.lineFoldingOnly else (Is_Set => False));

   function completion (X : TextDocumentClientCapabilities_Optional)
     return CompletionClientCapabilities_Optional is
       (if X.Is_Set then X.Value.completion else (Is_Set => False));

   function completionItem (X : CompletionClientCapabilities_Optional)
     return completionItem_OfCompletionClientCapabilities_Optional is
       (if X.Is_Set then X.Value.completionItem else (Is_Set => False));

   function resolveSupport
     (X : completionItem_OfCompletionClientCapabilities_Optional)
       return resolveSupport_OfWorkspaceSymbolClientCapabilities_Optional is
         (if X.Is_Set then X.Value.resolveSupport else (Is_Set => False));

   function properties
     (X : resolveSupport_OfWorkspaceSymbolClientCapabilities_Optional)
       return LSP.Structures.Virtual_String_Vector is
         (if X.Is_Set then X.Value.properties
          else VSS.String_Vectors.Empty_Virtual_String_Vector);

   function workspaceEdit (X : WorkspaceClientCapabilities_Optional)
     return WorkspaceEditClientCapabilities_Optional is
       (if X.Is_Set then X.Value.workspaceEdit else (Is_Set => False));

   function documentChanges (X : WorkspaceEditClientCapabilities_Optional)
     return Boolean_Optional is
       (if X.Is_Set then X.Value.documentChanges else (Is_Set => False));

   function resourceOperations (X : WorkspaceEditClientCapabilities_Optional)
     return LSP.Structures.ResourceOperationKind_Set is
       (if X.Is_Set then X.Value.resourceOperations else (others => False));

   function publishDiagnostics (X : TextDocumentClientCapabilities_Optional)
     return PublishDiagnosticsClientCapabilities_Optional is
       (if X.Is_Set then X.Value.publishDiagnostics else (Is_Set => False));

   function relatedInformation
     (X : PublishDiagnosticsClientCapabilities_Optional)
       return Boolean_Optional is
         (if X.Is_Set then X.Value.relatedInformation else (Is_Set => False));

   function codeAction (X : TextDocumentClientCapabilities_Optional)
     return CodeActionClientCapabilities_Optional is
       (if X.Is_Set then X.Value.codeAction else (Is_Set => False));

   function documentSymbol (X : TextDocumentClientCapabilities_Optional)
     return DocumentSymbolClientCapabilities_Optional is
       (if X.Is_Set then X.Value.documentSymbol else (Is_Set => False));

   function hierarchicalDocumentSymbolSupport
     (X : DocumentSymbolClientCapabilities_Optional)
       return Boolean_Optional is
         (if X.Is_Set then X.Value.hierarchicalDocumentSymbolSupport
          else (Is_Set => False));

   function didChangeWatchedFiles (X : WorkspaceClientCapabilities_Optional)
     return DidChangeWatchedFilesClientCapabilities_Optional is
       (if X.Is_Set then X.Value.didChangeWatchedFiles else (Is_Set => False));

   function dynamicRegistration
     (X : DidChangeWatchedFilesClientCapabilities_Optional)
       return Boolean_Optional is
         (if X.Is_Set then X.Value.dynamicRegistration else (Is_Set => False));

   function fileOperations (X : WorkspaceClientCapabilities_Optional)
     return FileOperationClientCapabilities_Optional is
       (if X.Is_Set then X.Value.fileOperations else (Is_Set => False));

   function dynamicRegistration
     (X : FileOperationClientCapabilities_Optional)
       return Boolean_Optional is
         (if X.Is_Set then X.Value.dynamicRegistration else (Is_Set => False));

   function didCreate
     (X : FileOperationClientCapabilities_Optional)
       return Boolean_Optional is
         (if X.Is_Set then X.Value.didCreate else (Is_Set => False));

   function didDelete
     (X : FileOperationClientCapabilities_Optional)
       return Boolean_Optional is
         (if X.Is_Set then X.Value.didDelete else (Is_Set => False));

   function didRename
     (X : FileOperationClientCapabilities_Optional)
       return Boolean_Optional is
         (if X.Is_Set then X.Value.didRename else (Is_Set => False));

end LSP.Structures.Unwrap;
