with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package A is

   package Integer_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Integer,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   package IHM renames Integer_Hashed_Maps;

   procedure Subprogram;

end A;
