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

   function True
     return LSP.Structures.typeDefinitionProvider_OfServerCapabilities
       is (Kind => LSP.Structures.Variant_1, Variant_1 => True);

   function True
     return LSP.Structures.typeDefinitionProvider_OfServerCapabilities_Optional
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

   function Empty return LSP.Structures.Position
     is (line => 0, character => 0);

   function Empty return LSP.Structures.A_Range
     is (start => Empty, an_end => Empty);

   function Error return LSP.Structures.DiagnosticSeverity_Optional
     is (Is_Set => True, Value => LSP.Enumerations.Error);

   function Empty
     return LSP.Structures.DocumentUri
       is ((VSS.Strings.Empty_Virtual_String with null record));

   ErrorCodes_Map : constant array (LSP.Enumerations.ErrorCodes) of Integer :=
     [LSP.Enumerations.ParseError => -32700,
      LSP.Enumerations.InvalidRequest => -32600,
      LSP.Enumerations.MethodNotFound => -32601,
      LSP.Enumerations.InvalidParams => -32602,
      LSP.Enumerations.InternalError => -32603,
      LSP.Enumerations.jsonrpcReservedErrorRangeStart => -32099,
      LSP.Enumerations.serverErrorStart => -32099,
      LSP.Enumerations.ServerNotInitialized => -32002,
      LSP.Enumerations.UnknownErrorCode => -32001,
      LSP.Enumerations.jsonrpcReservedErrorRangeEnd => -32000,
      LSP.Enumerations.serverErrorEnd => -32000];

   LSPErrorCodes_Map : constant array
     (LSP.Enumerations.LSPErrorCodes) of Integer :=
       [LSP.Enumerations.lspReservedErrorRangeStart => -32899,
        LSP.Enumerations.RequestFailed => -32803,
        LSP.Enumerations.ServerCancelled => -32802,
        LSP.Enumerations.ContentModified => -32801,
        LSP.Enumerations.RequestCancelled => -32800,
        LSP.Enumerations.lspReservedErrorRangeEnd => -32800];

   function InternalError return Integer
     is (ErrorCodes_Map (LSP.Enumerations.InternalError));

   function MethodNotFound return Integer
     is (ErrorCodes_Map (LSP.Enumerations.MethodNotFound));

   function InvalidRequest return Integer
     is (ErrorCodes_Map (LSP.Enumerations.InvalidRequest));

   function UnknownErrorCode return Integer
     is (ErrorCodes_Map (LSP.Enumerations.UnknownErrorCode));

   function ServerNotInitialized return Integer
     is (ErrorCodes_Map (LSP.Enumerations.ServerNotInitialized));

   function InvalidParams return Integer
     is (ErrorCodes_Map (LSP.Enumerations.InvalidParams));

   function RequestFailed return Integer
     is (LSPErrorCodes_Map (LSP.Enumerations.RequestFailed));

end LSP.Constants;
