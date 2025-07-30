package body Types is

   procedure FooBar is
      type Foo is new Integer;
      type Bar is new Foo;
      f: Foo := 5;
      b: Bar := 5;
   begin
   end FooBar;

end Types;