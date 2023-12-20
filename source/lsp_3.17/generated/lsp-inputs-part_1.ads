--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

package LSP.Inputs.Part_1 is

   procedure Read_NotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentIdentifier);

   procedure Read_SignatureHelp
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelp);

   procedure Read_ShowDocumentResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowDocumentResult);

   procedure Read_CompletionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionRegistrationOptions);

   procedure Read_SignatureHelpTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SignatureHelpTriggerKind);

   procedure Read_MonikerParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerParams);

   procedure Read_DiagnosticTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DiagnosticTag);

   procedure Read_WorkspaceSymbol_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbol_Vector);

   procedure Read_SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult);

   procedure Read_SignatureHelpOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpOptions);

   procedure Read_DidChangeNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeNotebookDocumentParams);

   procedure Read_DocumentSymbolParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolParams);

   procedure Read_WorkspaceDocumentDiagnosticReport_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDocumentDiagnosticReport_Vector);

end LSP.Inputs.Part_1;
