--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Content_Handlers;
with VSS.Strings;

with LSP.Structures;

package LSP.Output_Tools is
   pragma Preelaborate;

   procedure Write_LSPAny
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LSPAny);

   procedure Write_Start_Notification
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Method  : VSS.Strings.Virtual_String);

   procedure Write_Start_Request
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Method  : VSS.Strings.Virtual_String;
      Id      : LSP.Structures.Integer_Or_Virtual_String);

   procedure Write_Start_Response
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Id      : LSP.Structures.Integer_Or_Virtual_String);

   procedure Write_Start_Progress_Report
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Token   : LSP.Structures.ProgressToken);

end LSP.Output_Tools;
