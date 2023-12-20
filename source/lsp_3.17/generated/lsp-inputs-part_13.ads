--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

package LSP.Inputs.Part_13 is

   procedure Read_Null_Record
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Null_Record);

   procedure Read_NotebookCell_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCell_Vector);

   procedure Read_LSPAny
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPAny);

   procedure Read_SymbolTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolTag_Set);

   procedure Read_GeneralClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.GeneralClientCapabilities);

   procedure Read_CodeLens_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLens_Vector_Or_Null);

   procedure Read_HoverClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverClientCapabilities);

   procedure Read_InsertTextFormat
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InsertTextFormat);

   procedure Read_DocumentSymbolOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolOptions);

   procedure Read_Tokens_Delta_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Tokens_Delta_Result);

   procedure Read_MonikerOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerOptions);

   procedure Read_TypeHierarchySupertypesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchySupertypesParams);

   procedure Read_Moniker
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Moniker);

   procedure Read_MarkupContent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkupContent);

   procedure Read_WorkspaceEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceEdit);

   procedure Read_InsertTextMode
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InsertTextMode);

   procedure Read_CompletionItemKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionItemKind);

   procedure Read_Command_Or_CodeAction
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command_Or_CodeAction);

   procedure Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult);

   procedure Read_DocumentSymbol
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol);

   procedure Read_DocumentHighlight_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlight_Vector);

   procedure Read_SignatureHelpParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpParams);

   procedure Read_Position
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Position);

   procedure Read_ExecuteCommandRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandRegistrationOptions);

   procedure Read_LSPErrorCodes
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.LSPErrorCodes);

end LSP.Inputs.Part_13;
