--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Pull_Readers;
with VSS.Strings;
with LSP.Client_Responses;

package LSP.Client_Response_Readers is

   procedure Initialize;

   function Read_Response
     (Input  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method : VSS.Strings.Virtual_String)
      return LSP.Client_Responses.Client_Response'Class;

end LSP.Client_Response_Readers;
