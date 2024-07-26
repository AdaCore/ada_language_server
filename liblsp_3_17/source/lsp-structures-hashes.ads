--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

private with VSS.Strings.Hash;

package LSP.Structures.Hashes is

   function Hash
     (Item : LSP.Structures.Integer_Or_Virtual_String)
      return Ada.Containers.Hash_Type;

private

   function Hash
     (Item : LSP.Structures.Integer_Or_Virtual_String)
      return Ada.Containers.Hash_Type is
        (case Item.Is_Integer is
            when True  => Ada.Containers.Hash_Type'Mod (Item.Integer),
            when False => VSS.Strings.Hash (Item.Virtual_String));

end LSP.Structures.Hashes;
