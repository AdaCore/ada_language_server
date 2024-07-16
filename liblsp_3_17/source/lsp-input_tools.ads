--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Pull_Readers;
with VSS.Strings;

with LSP.Structures;

package LSP.Input_Tools is

   procedure Read_LSPAny
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPAny);

   procedure Read_LSPAny_Class
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.JSON_Event_Vectors.Vector'Class);

   procedure Look_For_MarkupContent_Or_MarkedString_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkupContent_Or_MarkedString_Vector);

   generic
      type Parameter_Type is private;

      Method : VSS.Strings.Virtual_String;

      with procedure Read_Parameter
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out Parameter_Type);

   procedure Read_Notification
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Parameter_Type);

   generic
      type Parameter_Type is private;

      Method : VSS.Strings.Virtual_String;

      with procedure Read_Parameter
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out Parameter_Type);

   procedure Read_Request
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : out LSP.Structures.Integer_Or_Virtual_String;
      Value   : out Parameter_Type);

   procedure Read_Null_Request
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method  : VSS.Strings.Virtual_String;
      Id      : out LSP.Structures.Integer_Or_Virtual_String);

   generic
      type Result_Type is private;

      with procedure Read_Result
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out Result_Type);

   procedure Read_Response
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : out LSP.Structures.Integer_Or_Virtual_String;
      Value   : out Result_Type);

   generic
      type Result_Type is private;

      with procedure Read_Progress_Report
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out Result_Type);

   procedure Read_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Token   : out LSP.Structures.ProgressToken;
      Value   : out Result_Type);

end LSP.Input_Tools;
