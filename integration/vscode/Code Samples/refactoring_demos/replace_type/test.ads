package Test is

   type Foo;

   type Foo is new Integer;

   type Baz is new Integer;

   type Bar is new Foo;

   F : constant Foo := 1;

   B : constant Bar := 1;

end Test;
