--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Pull_Readers;
with LSP.Errors;

procedure LSP.Inputs.Read_ResponseError
  (Stream : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
   Value  : out LSP.Errors.ResponseError);
