package My_Package is
   type Foo is record I : Integer; end record;
   type Bar is record F : Foo; end record;
   type Qux is record B : Bar; end record;
   type Corge is record Q : Qux; end record;
   C1 : constant Corge := (Q => (B => (F => (I => 1))));
   C2 : constant Corge := (Q => (B => (F => (I => 2))));
end My_Package;
