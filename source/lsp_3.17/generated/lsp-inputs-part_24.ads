--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

package LSP.Inputs.Part_24 is

   procedure Read_DidSaveNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidSaveNotebookDocumentParams);

   procedure Read_CodeActionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionRegistrationOptions);

   procedure Read_InlineValue
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValue);

   procedure Read_TypeDefinitionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionClientCapabilities);

   procedure Read_CreateFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFileOptions);

   procedure Read_InlineValue_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValue_Vector_Or_Null);

   procedure Read_PublishDiagnosticsClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PublishDiagnosticsClientCapabilities);

   procedure Read_SemanticTokensOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensOptions);

   procedure Read_FoldingRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeParams);

   procedure Read_TextDocumentSaveRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSaveRegistrationOptions);

end LSP.Inputs.Part_24;
