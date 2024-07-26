--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package LSP.Server_Responses.Errors
  with Preelaborate
is

   type Response is new LSP.Server_Responses.Server_Response with record
      Error : LSP.Errors.ResponseError;
   end record;

   overriding procedure Visit_Server_Receiver
     (Self  : Response;
      Value : in out
        LSP.Server_Response_Receivers.Server_Response_Receiver'Class);

end LSP.Server_Responses.Errors;
