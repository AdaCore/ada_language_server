--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Server_Requests;
with VSS.JSON.Pull_Readers;
with VSS.Strings;

package LSP.Server_Request_Readers is

   procedure Initialize;

   function Read_Request
     (Input  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method : VSS.Strings.Virtual_String)
      return LSP.Server_Requests.Server_Request'Class;

end LSP.Server_Request_Readers;
