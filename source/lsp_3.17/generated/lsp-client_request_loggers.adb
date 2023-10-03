--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with VSS.Strings;

package body LSP.Client_Request_Loggers is

   overriding procedure On_RegisterCapability_Request
     (Self  : in out Client_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RegistrationParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'client/registerCapability'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_RegisterCapability_Request;

   overriding procedure On_UnregisterCapability_Request
     (Self  : in out Client_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.UnregistrationParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'client/unregisterCapability'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_UnregisterCapability_Request;

   overriding procedure On_ShowDocument_Request
     (Self  : in out Client_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowDocumentParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'window/showDocument'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_ShowDocument_Request;

   overriding procedure On_ShowMessageRequest_Request
     (Self  : in out Client_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowMessageRequestParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'window/showMessageRequest'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_ShowMessageRequest_Request;

   overriding procedure On_Progress_Create_Request
     (Self  : in out Client_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressCreateParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'window/workDoneProgress/create'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Progress_Create_Request;

   overriding procedure On_ApplyEdit_Request
     (Self  : in out Client_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ApplyWorkspaceEditParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/applyEdit'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_ApplyEdit_Request;

   overriding procedure On_Code_Lens_Refresh_Request
     (Self : in out Client_Request_Logger;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/codeLens/refresh'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.New_Line (Ok);
   end On_Code_Lens_Refresh_Request;

   overriding procedure On_Configuration_Request
     (Self  : in out Client_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ConfigurationParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/configuration'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Configuration_Request;

   overriding procedure On_Diagnostic_Refresh_Request
     (Self : in out Client_Request_Logger;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/diagnostic/refresh'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.New_Line (Ok);
   end On_Diagnostic_Refresh_Request;

   overriding procedure On_Inlay_Refresh_Request
     (Self : in out Client_Request_Logger;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/inlayHint/refresh'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.New_Line (Ok);
   end On_Inlay_Refresh_Request;

   overriding procedure On_Inline_Refresh_Request
     (Self : in out Client_Request_Logger;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/inlineValue/refresh'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.New_Line (Ok);
   end On_Inline_Refresh_Request;

   overriding procedure On_Tokens_Refresh_Request
     (Self : in out Client_Request_Logger;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/semanticTokens/refresh'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.New_Line (Ok);
   end On_Tokens_Refresh_Request;

   overriding procedure On_WorkspaceFolders_Request
     (Self : in out Client_Request_Logger;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/workspaceFolders'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.New_Line (Ok);
   end On_WorkspaceFolders_Request;

   procedure Put_Id
     (Self : in out Client_Request_Logger'Class;
      Id   : LSP.Structures.Integer_Or_Virtual_String;
      Ok   : in out Boolean) is
   begin
      Self.Output.Put (" Id=", Ok);

      if Id.Is_Integer then
         Self.Output.Put
           (VSS.Strings.To_Virtual_String (Id.Integer'Wide_Wide_Image), Ok);
      else
         Self.Output.Put (Id.Virtual_String, Ok);
      end if;
   end Put_Id;

end LSP.Client_Request_Loggers;
