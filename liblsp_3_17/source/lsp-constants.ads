--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Streams;
with VSS.Strings;

with LSP.Enumerations;
with LSP.Structures;

package LSP.Constants is
   pragma Preelaborate;

   function False return LSP.Structures.Boolean_Optional
     is (Is_Set => True, Value => False);

   function True return LSP.Structures.Boolean_Optional
     is (Is_Set => True, Value => True);

   function True
     return LSP.Structures.foldingRangeProvider_OfServerCapabilities
       is (Kind => LSP.Structures.Variant_1, Variant_1 => True);

   function True
     return LSP.Structures.foldingRangeProvider_OfServerCapabilities_Optional
       is (Is_Set => True, Value => True);

   function True
     return LSP.Structures.implementationProvider_OfServerCapabilities
       is (Kind => LSP.Structures.Variant_1, Variant_1 => True);

   function True
     return LSP.Structures.implementationProvider_OfServerCapabilities_Optional
       is (Is_Set => True, Value => True);

   function True
     return LSP.Structures.declarationProvider_OfServerCapabilities
       is (Kind => LSP.Structures.Variant_1, Variant_1 => True);

   function True
     return LSP.Structures.declarationProvider_OfServerCapabilities_Optional
       is (Is_Set => True, Value => True);

   function True
     return LSP.Structures.Boolean_Or_DefinitionOptions
       is (Is_Boolean => True, Boolean => True);

   function True
     return LSP.Structures.Boolean_Or_DefinitionOptions_Optional
       is (Is_Set => True, Value => True);

   function True
     return LSP.Structures.callHierarchyProvider_OfServerCapabilities
       is (Kind => LSP.Structures.Variant_1, Variant_1 => True);

   function True
     return LSP.Structures.callHierarchyProvider_OfServerCapabilities_Optional
       is (Is_Set => True, Value => True);

   function True return LSP.Structures.Boolean_Or_HoverOptions
     is (Is_Boolean => True, Boolean => True);

   function True return LSP.Structures.Boolean_Or_HoverOptions_Optional
     is (Is_Set => True, Value => True);

   function True return LSP.Structures.Boolean_Or_DocumentSymbolOptions
     is (Is_Boolean => True, Boolean => True);

   function True
     return LSP.Structures.Boolean_Or_DocumentSymbolOptions_Optional
       is (Is_Set => True, Value => True);

   function True return LSP.Structures.Boolean_Or_CodeActionOptions
     is (Is_Boolean => True, Boolean => True);

   function True return LSP.Structures.Boolean_Or_CodeActionOptions_Optional
     is (Is_Set => True, Value => True);

   function True
     return LSP.Structures.typeDefinitionProvider_OfServerCapabilities
       is (Kind => LSP.Structures.Variant_1, Variant_1 => True);

   function True
     return LSP.Structures.typeDefinitionProvider_OfServerCapabilities_Optional
       is (Is_Set => True, Value => True);

   function True
     return LSP.Structures.typeHierarchyProvider_OfServerCapabilities
       is (Kind => LSP.Structures.Variant_1, Variant_1 => True);

   function True
     return LSP.Structures.typeHierarchyProvider_OfServerCapabilities_Optional
       is (Is_Set => True, Value => True);

   function True return LSP.Structures.Boolean_Or_WorkspaceSymbolOptions
     is (Is_Boolean => True, Boolean => True);

   function True
     return LSP.Structures.Boolean_Or_WorkspaceSymbolOptions_Optional
       is (Is_Set => True, Value => True);

   function True return LSP.Structures.Boolean_Or_Something
     is (Is_Boolean => True, Boolean => True);

   function True return LSP.Structures.Boolean_Or_Something_Optional
     is (Is_Set => True, Value => True);

   function True return LSP.Structures.Boolean_Or_Any
     is [(VSS.JSON.Streams.Boolean_Value, True)];

   function True return LSP.Structures.Boolean_Or_Any_Optional
     is (Is_Set => True, Value => True);

   function True return LSP.Structures.Boolean_Or_ReferenceOptions
     is (Is_Boolean => True, Boolean => True);

   function True return LSP.Structures.Boolean_Or_ReferenceOptions_Optional
     is (Is_Set => True, Value => True);

   function True
     return LSP.Structures.Boolean_Or_DocumentHighlightOptions_Optional
      is (Is_Set => True, Value => (Is_Boolean => True, Boolean => True));

   function True
     return LSP.Structures.Boolean_Or_DocumentFormattingOptions_Optional
      is (Is_Set => True, Value => (Is_Boolean => True, Boolean => True));

   function True
     return LSP.Structures.Boolean_Or_DocumentRangeFormattingOptions_Optional
      is (Is_Set => True, Value => (Is_Boolean => True, Boolean => True));

   function Empty return LSP.Structures.Position
      is (line => 0, character => 0);

   function Empty return LSP.Structures.A_Range
     is (start => Empty, an_end => Empty);

   function Empty
     return LSP.Structures.DocumentUri
       is ((VSS.Strings.Empty_Virtual_String with null record));

   function Empty return LSP.Structures.SymbolTag_Set is [others => False];

   function Empty return LSP.Structures.AlsReferenceKind_Set is
     [others => False];

   function Error return LSP.Structures.DiagnosticSeverity_Optional
     is (Is_Set => True, Value => LSP.Enumerations.Error);

   function RequestFailed return LSP.Enumerations.ErrorCodes is
     (LSP.Enumerations.ErrorCodes (LSP.Enumerations.RequestFailed));

   function RequestCancelled return LSP.Enumerations.ErrorCodes is
     (LSP.Enumerations.ErrorCodes (LSP.Enumerations.RequestCancelled));

   function file return LSP.Structures.FileOperationPatternKind_Optional is
     (Is_Set => True, Value => LSP.Enumerations.file);

   function ignoreCase
     return LSP.Structures.FileOperationPatternOptions_Optional is
       (Is_Set => True, Value => (ignoreCase => True));

   function Region return LSP.Structures.FoldingRangeKind_Optional is
     (Is_Set => True, Value => LSP.Enumerations.Region);

   function Imports return LSP.Structures.FoldingRangeKind_Optional is
     (Is_Set => True, Value => LSP.Enumerations.Imports);

   function Comment return LSP.Structures.FoldingRangeKind_Optional is
     (Is_Set => True, Value => LSP.Enumerations.Comment);

end LSP.Constants;
