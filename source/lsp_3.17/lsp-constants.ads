--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Structures;

package LSP.Constants is
   pragma Preelaborate;

   function False return LSP.Structures.Boolean_Optional
     is (Is_Set => True, Value => False);

   function True return LSP.Structures.Boolean_Optional
     is (Is_Set => True, Value => True);

   function True
     return LSP.Structures.foldingRangeProvider_OfServerCapabilities
       is (Kind => LSP.Structures.Varian_1, Varian_1 => True);

   function True
     return LSP.Structures.foldingRangeProvider_OfServerCapabilities_Optional
       is (Is_Set => True, Value => True);

   function True
     return LSP.Structures.implementationProvider_OfServerCapabilities
       is (Kind => LSP.Structures.Varian_1, Varian_1 => True);

   function True
     return LSP.Structures.implementationProvider_OfServerCapabilities_Optional
       is (Is_Set => True, Value => True);

   function True
     return LSP.Structures.declarationProvider_OfServerCapabilities
       is (Kind => LSP.Structures.Varian_1, Varian_1 => True);

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
       is (Kind => LSP.Structures.Varian_1, Varian_1 => True);

   function True
     return LSP.Structures.callHierarchyProvider_OfServerCapabilities_Optional
       is (Is_Set => True, Value => True);

   function True
     return LSP.Structures.typeDefinitionProvider_OfServerCapabilities
       is (Kind => LSP.Structures.Varian_1, Varian_1 => True);

   function True
     return LSP.Structures.typeDefinitionProvider_OfServerCapabilities_Optional
       is (Is_Set => True, Value => True);

end LSP.Constants;
