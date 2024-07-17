--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Streams;

with LSP.Outputs;

package body LSP.Output_Tools is

   ------------------
   -- Write_LSPAny --
   ------------------

   procedure Write_LSPAny
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LSPAny) is
   begin
      for Item of Value loop
         case Item.Kind is
            when VSS.JSON.Streams.None
                 | VSS.JSON.Streams.Invalid
                 | VSS.JSON.Streams.Start_Document
                 | VSS.JSON.Streams.End_Document
                 | VSS.JSON.Streams.Comment =>
               raise Program_Error;
            when VSS.JSON.Streams.Start_Array =>
               Handler.Start_Array;
            when VSS.JSON.Streams.End_Array =>
               Handler.End_Array;
            when VSS.JSON.Streams.Start_Object =>
               Handler.Start_Object;
            when VSS.JSON.Streams.End_Object =>
               Handler.End_Object;
            when VSS.JSON.Streams.Key_Name =>
               Handler.Key_Name (Item.Key_Name);
            when VSS.JSON.Streams.String_Value =>
               Handler.String_Value (Item.String_Value);
            when VSS.JSON.Streams.Number_Value =>
               Handler.Number_Value (Item.Number_Value);
            when VSS.JSON.Streams.Boolean_Value =>
               Handler.Boolean_Value (Item.Boolean_Value);
            when VSS.JSON.Streams.Null_Value =>
               Handler.Null_Value;
         end case;
      end loop;
   end Write_LSPAny;

   ------------------------------
   -- Write_Start_Notification --
   ------------------------------

   procedure Write_Start_Notification
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Method  : VSS.Strings.Virtual_String) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("jsonrpc");
      Handler.String_Value ("2.0");
      Handler.Key_Name ("method");
      Handler.String_Value (Method);
   end Write_Start_Notification;

   --------------------------------
   -- Write_Start_Progress_Report --
   --------------------------------

   procedure Write_Start_Progress_Report
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Token   : LSP.Structures.ProgressToken) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("jsonrpc");
      Handler.String_Value ("2.0");
      Handler.Key_Name ("method");
      Handler.String_Value ("$/progress");
      Handler.Key_Name ("params");
      Handler.Start_Object;
      Handler.Key_Name ("token");
      LSP.Outputs.Write_ProgressToken (Handler, Token);
      Handler.Key_Name ("value");
   end Write_Start_Progress_Report;

   -------------------------
   -- Write_Start_Request --
   -------------------------

   procedure Write_Start_Request
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Method  : VSS.Strings.Virtual_String;
      Id      : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("jsonrpc");
      Handler.String_Value ("2.0");
      Handler.Key_Name ("id");
      LSP.Outputs.Write_Integer_Or_Virtual_String (Handler, Id);
      Handler.Key_Name ("method");
      Handler.String_Value (Method);
   end Write_Start_Request;

   --------------------------
   -- Write_Start_Response --
   --------------------------

   procedure Write_Start_Response
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Id      : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("jsonrpc");
      Handler.String_Value ("2.0");
      Handler.Key_Name ("id");
      LSP.Outputs.Write_Integer_Or_Virtual_String (Handler, Id);
   end Write_Start_Response;

end LSP.Output_Tools;
