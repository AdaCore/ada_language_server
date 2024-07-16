--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

package LSP.Inputs.Part_17 is

   procedure Read_WorkspaceFolder_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFolder_Vector);

   procedure Read_SemanticTokenModifiers
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SemanticTokenModifiers);

   procedure Read_ConfigurationItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ConfigurationItem);

   procedure Read_Definition
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Definition);

   procedure Read_CallHierarchyOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOptions);

   procedure Read_DefinitionLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionLink);

   procedure Read_DefinitionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionOptions);

   procedure Read_FullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FullDocumentDiagnosticReport);

   procedure Read_ParameterInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ParameterInformation);

   procedure Read_Symbol_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Symbol_Progress_Report);

   procedure Read_CodeLensParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensParams);

   procedure Read_PreviousResultId
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PreviousResultId);

   procedure Read_TextDocumentChangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentChangeRegistrationOptions);

   procedure Read_AlsDisplayMethodAncestryOnNavigationPolicy
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Enumerations.AlsDisplayMethodAncestryOnNavigationPolicy);

   procedure Read_DidCloseNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidCloseNotebookDocumentParams);

   procedure Read_DiagnosticServerCancellationData
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticServerCancellationData);

   procedure Read_ShowMessageRequestParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowMessageRequestParams);

   procedure Read_CompletionItemLabelDetails
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItemLabelDetails);

   procedure Read_TextDocumentSyncOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSyncOptions);

end LSP.Inputs.Part_17;
