--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with VSS.Strings;

package body LSP.Server_Response_Loggers is

   overriding procedure On_RegisterCapability_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'client/registerCapability'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_RegisterCapability_Response;

   overriding procedure On_UnregisterCapability_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'client/unregisterCapability'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_UnregisterCapability_Response;

   overriding procedure On_ShowDocument_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowDocumentResult) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'window/showDocument'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_ShowDocument_Response;

   overriding procedure On_ShowMessageRequest_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.MessageActionItem_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'window/showMessageRequest'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_ShowMessageRequest_Response;

   overriding procedure On_Progress_Create_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'window/workDoneProgress/create'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Progress_Create_Response;

   overriding procedure On_ApplyEdit_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ApplyWorkspaceEditResult) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/applyEdit'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_ApplyEdit_Response;

   overriding procedure On_Code_Lens_Refresh_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/codeLens/refresh'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Code_Lens_Refresh_Response;

   overriding procedure On_Configuration_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LSPAny_Vector) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/configuration'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Configuration_Response;

   overriding procedure On_Diagnostic_Refresh_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/diagnostic/refresh'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Diagnostic_Refresh_Response;

   overriding procedure On_Inlay_Refresh_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/inlayHint/refresh'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Inlay_Refresh_Response;

   overriding procedure On_Inline_Refresh_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/inlineValue/refresh'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Inline_Refresh_Response;

   overriding procedure On_Tokens_Refresh_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/semanticTokens/refresh'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Tokens_Refresh_Response;

   overriding procedure On_WorkspaceFolders_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceFolder_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/workspaceFolders'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_WorkspaceFolders_Response;

   overriding procedure On_Error_Response
     (Self  : in out Server_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Errors.ResponseError) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'Error response'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" error : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Error_Response;

   procedure Put_Id
     (Self : in out Server_Response_Logger'Class;
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

end LSP.Server_Response_Loggers;
