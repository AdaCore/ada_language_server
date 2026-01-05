package Pkg is
   type T is private;
   procedure Print (Item : T);
private
   type T is (Some, Enum);
end Pkg;