--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

package LSP.Errors is
   pragma Preelaborate;

   type ResponseError is record
      code : Integer;
      --  A number indicating the error type that occurred.
      message : VSS.Strings.Virtual_String;
      --  A string providing a short description of the error.

      --  data: string | number | boolean | array | object | null;
      --  A primitive or structured value that contains additional information
      --  about the error. Can be omitted.
   end record;

   type ResponseError_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : ResponseError;
      end case;
   end record;

end LSP.Errors;
