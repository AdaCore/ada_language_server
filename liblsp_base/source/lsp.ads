--
--  Copyright (C) 2018-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package LSP is
   pragma Pure;

   --  XXX Below is obsolete code for LSP 3.16 only. Please remove it when
   --  transition to newer version has been completed.

   type On_Empty_Array is (Skip, Write_Array, Write_Null);
   --  Configuration of array writting procedure

end LSP;
