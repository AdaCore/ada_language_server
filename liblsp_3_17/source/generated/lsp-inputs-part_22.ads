--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

package LSP.Inputs.Part_22 is

   procedure Read_Declaration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Declaration);

   procedure Read_RelatedUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RelatedUnchangedDocumentDiagnosticReport);

   procedure Read_CallHierarchyItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem);

   procedure Read_InlayHintKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InlayHintKind);

   procedure Read_RenameOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameOptions);

   procedure Read_DidChangeConfigurationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeConfigurationClientCapabilities);

   procedure Read_DeclarationLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationLink);

   procedure Read_Virtual_String_Or_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String_Or_NotebookDocumentFilter);

   procedure Read_CallHierarchyItem_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem_Vector_Or_Null);

   procedure Read_DocumentOnTypeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DocumentOnTypeFormattingRegistrationOptions);

   procedure Read_DocumentHighlightClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightClientCapabilities);

   procedure Read_Integer_Or_Virtual_String
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Integer_Or_Virtual_String);

   procedure Read_AlsReferenceKind_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.AlsReferenceKind_Set);

end LSP.Inputs.Part_22;
