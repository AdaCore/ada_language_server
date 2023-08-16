--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package LSP.Structures.Unwrap is
   pragma Preelaborate;

   function foldingRange (X : TextDocumentClientCapabilities_Optional)
     return FoldingRangeClientCapabilities_Optional is
       (if X.Is_Set then X.Value.foldingRange else (Is_Set => False));

   function lineFoldingOnly (X : FoldingRangeClientCapabilities_Optional)
     return Boolean_Optional is
       (if X.Is_Set then X.Value.lineFoldingOnly else (Is_Set => False));

end LSP.Structures.Unwrap;
