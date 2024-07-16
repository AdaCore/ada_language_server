--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

package LSP.Inputs.Part_8 is

   procedure Read_FileOperationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationRegistrationOptions);

   procedure Read_CreateFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFile);

   procedure Read_SemanticTokens
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokens);

   procedure Read_WorkDoneProgressReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressReport);

   procedure Read_Boolean_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Boolean_Vector);

   procedure Read_WorkspaceSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolRegistrationOptions);

   procedure Read_PositionEncodingKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.PositionEncodingKind);

   procedure Read_Completion_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Completion_Result);

   procedure Read_FileCreate
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileCreate);

   procedure Read_InlineValueOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueOptions);

   procedure Read_ImplementationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationOptions);

   procedure Read_ChangeAnnotationIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ChangeAnnotationIdentifier);

   procedure Read_InlayHintWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintWorkspaceClientCapabilities);

   procedure Read_DocumentColorOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorOptions);

   procedure Read_TypeHierarchyPrepareParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyPrepareParams);

   procedure Read_TextDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentFilter);

   procedure Read_LinkedEditingRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeOptions);

end LSP.Inputs.Part_8;
