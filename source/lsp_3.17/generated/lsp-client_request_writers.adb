--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Output_Tools;
with LSP.Outputs;

package body LSP.Client_Request_Writers is

   overriding procedure On_RegisterCapability_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RegistrationParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "client/registerCapability", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_RegistrationParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_RegisterCapability_Request;

   overriding procedure On_UnregisterCapability_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.UnregistrationParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "client/unregisterCapability", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_UnregistrationParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_UnregisterCapability_Request;

   overriding procedure On_ShowDocument_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowDocumentParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "window/showDocument", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_ShowDocumentParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_ShowDocument_Request;

   overriding procedure On_ShowMessageRequest_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowMessageRequestParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "window/showMessageRequest", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_ShowMessageRequestParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_ShowMessageRequest_Request;

   overriding procedure On_Progress_Create_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressCreateParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "window/workDoneProgress/create", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_WorkDoneProgressCreateParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Progress_Create_Request;

   overriding procedure On_ApplyEdit_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ApplyWorkspaceEditParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/applyEdit", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_ApplyWorkspaceEditParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_ApplyEdit_Request;

   overriding procedure On_Code_Lens_Refresh_Request
     (Self : in out Client_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/codeLens/refresh", Id);
      Self.Output.End_Object;
   end On_Code_Lens_Refresh_Request;

   overriding procedure On_Configuration_Request
     (Self  : in out Client_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ConfigurationParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/configuration", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_ConfigurationParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Configuration_Request;

   overriding procedure On_Diagnostic_Refresh_Request
     (Self : in out Client_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/diagnostic/refresh", Id);
      Self.Output.End_Object;
   end On_Diagnostic_Refresh_Request;

   overriding procedure On_Inlay_Refresh_Request
     (Self : in out Client_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/inlayHint/refresh", Id);
      Self.Output.End_Object;
   end On_Inlay_Refresh_Request;

   overriding procedure On_Inline_Refresh_Request
     (Self : in out Client_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/inlineValue/refresh", Id);
      Self.Output.End_Object;
   end On_Inline_Refresh_Request;

   overriding procedure On_Tokens_Refresh_Request
     (Self : in out Client_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/semanticTokens/refresh", Id);
      Self.Output.End_Object;
   end On_Tokens_Refresh_Request;

   overriding procedure On_WorkspaceFolders_Request
     (Self : in out Client_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/workspaceFolders", Id);
      Self.Output.End_Object;
   end On_WorkspaceFolders_Request;

end LSP.Client_Request_Writers;
