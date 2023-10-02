--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.String_Vectors;
with VSS.Strings;

generic
   Variants : VSS.String_Vectors.Virtual_String_Vector;

package Minimal_Perfect_Hash is
   --  pragma Preelaborate;

   function Get_Index (Text : VSS.Strings.Virtual_String) return Natural;
   --  Return index of Text in Variants or zero if Variants doesn't containt
   --  Text.

   procedure Initialize (Seed : Natural := 0);
   --  Performe internal initialization.

end Minimal_Perfect_Hash;
