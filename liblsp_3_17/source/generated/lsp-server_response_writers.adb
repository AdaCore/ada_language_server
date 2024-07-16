--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with Interfaces;

with LSP.Output_Tools;
with LSP.Outputs;

package body LSP.Server_Response_Writers is

   overriding procedure On_RegisterCapability_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Null_Record (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_RegisterCapability_Response;

   overriding procedure On_UnregisterCapability_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Null_Record (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_UnregisterCapability_Response;

   overriding procedure On_ShowDocument_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowDocumentResult) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_ShowDocumentResult (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_ShowDocument_Response;

   overriding procedure On_ShowMessageRequest_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.MessageActionItem_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_MessageActionItem_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_ShowMessageRequest_Response;

   overriding procedure On_Progress_Create_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Null_Record (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Progress_Create_Response;

   overriding procedure On_ApplyEdit_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ApplyWorkspaceEditResult) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_ApplyWorkspaceEditResult (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_ApplyEdit_Response;

   overriding procedure On_Code_Lens_Refresh_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Null_Record (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Code_Lens_Refresh_Response;

   overriding procedure On_Configuration_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LSPAny_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_LSPAny_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Configuration_Response;

   overriding procedure On_Diagnostic_Refresh_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Null_Record (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Diagnostic_Refresh_Response;

   overriding procedure On_Inlay_Refresh_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Null_Record (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Inlay_Refresh_Response;

   overriding procedure On_Inline_Refresh_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Null_Record (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Inline_Refresh_Response;

   overriding procedure On_Tokens_Refresh_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Null_Record (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Tokens_Refresh_Response;

   overriding procedure On_WorkspaceFolders_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceFolder_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_WorkspaceFolder_Vector_Or_Null
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_WorkspaceFolders_Response;

   overriding procedure On_Error_Response
     (Self  : in out Server_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Errors.ResponseError) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("error");
      Self.Output.Start_Object;
      Self.Output.Key_Name ("code");
      Self.Output.Integer_Value (Interfaces.Integer_64'Val (Value.code));
      Self.Output.Key_Name ("message");
      Self.Output.String_Value (Value.message);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Error_Response;

end LSP.Server_Response_Writers;
