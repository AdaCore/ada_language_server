procedure Foo is

   generic
   package Foo_Bar is
      procedure Do_Nothing is null;
      --  It doesn't do anything, but test documentation extraction.

   end Foo_Bar;

begin
   Foo_Bar.Do_Nothing
end Foo;
