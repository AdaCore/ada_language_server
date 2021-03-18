procedure Foo 
is
   procedure Bar (A : Integer; B, C : Integer);
   function FooBar (A : Integer) return Integer;

   ---------
   -- Bar --
   ---------

   procedure Bar (A : Integer; B, C : Integer) is
   begin
      null;
   end Bar;

   ------------
   -- FooBar --
   ------------

   function FooBar (A : Integer) return Integer is
   begin
      return A;
   end FooBar;
begin
   Bar (1, FooBar (2), 3);
end Foo;
