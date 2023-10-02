--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Pull_Readers;
with VSS.Strings;
with LSP.Server_Responses;

package LSP.Server_Response_Readers is

   procedure Initialize;

   function Read_Response
     (Input  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method : VSS.Strings.Virtual_String)
      return LSP.Server_Responses.Server_Response'Class;

end LSP.Server_Response_Readers;
