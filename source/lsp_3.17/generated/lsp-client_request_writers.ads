--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Structures;
with VSS.JSON.Content_Handlers;
with LSP.Client_Request_Receivers;

package LSP.Client_Request_Writers is
   pragma Preelaborate;

   type Client_Request_Writer
     (Output : access VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   new LSP.Client_Request_Receivers.Client_Request_Receiver with null record;

   overriding procedure On_RegisterCapability_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RegistrationParams);

   overriding procedure On_UnregisterCapability_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.UnregistrationParams);

   overriding procedure On_ShowDocument_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowDocumentParams);

   overriding procedure On_ShowMessageRequest_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowMessageRequestParams);

   overriding procedure On_Progress_Create_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressCreateParams);

   overriding procedure On_ApplyEdit_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ApplyWorkspaceEditParams);

   overriding procedure On_Code_Lens_Refresh_Request
     (Self : in out Client_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_Configuration_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ConfigurationParams);

   overriding procedure On_Diagnostic_Refresh_Request
     (Self : in out Client_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_Inlay_Refresh_Request
     (Self : in out Client_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_Inline_Refresh_Request
     (Self : in out Client_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_Tokens_Refresh_Request
     (Self : in out Client_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_WorkspaceFolders_Request
     (Self : in out Client_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

end LSP.Client_Request_Writers;
