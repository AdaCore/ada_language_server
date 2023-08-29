--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with VSS.JSON.Content_Handlers;

with LSP.Errors;
with LSP.Structures;
with LSP.Server_Response_Receivers;

package LSP.Server_Response_Writers is
   pragma Preelaborate;

   type Server_Response_Writer
     (Output : access VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   new LSP.Server_Response_Receivers.Server_Response_Receiver with null record;

   overriding procedure On_RegisterCapability_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record);

   overriding procedure On_UnregisterCapability_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record);

   overriding procedure On_ShowDocument_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowDocumentResult);

   overriding procedure On_ShowMessageRequest_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.MessageActionItem_Or_Null);

   overriding procedure On_Progress_Create_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record);

   overriding procedure On_ApplyEdit_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ApplyWorkspaceEditResult);

   overriding procedure On_Code_Lens_Refresh_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record);

   overriding procedure On_Configuration_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LSPAny_Vector);

   overriding procedure On_Diagnostic_Refresh_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record);

   overriding procedure On_Inlay_Refresh_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record);

   overriding procedure On_Inline_Refresh_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record);

   overriding procedure On_Tokens_Refresh_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record);

   overriding procedure On_WorkspaceFolders_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceFolder_Vector_Or_Null);

   overriding procedure On_Error_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Errors.ResponseError);

end LSP.Server_Response_Writers;
