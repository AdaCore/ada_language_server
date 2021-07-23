procedure Foo 
is
   procedure Bar (A : Integer; B, C : Integer) is null;

   function FooBar (A : Integer) return Integer is (A);

begin
   Bar 
   FooBar 
end Foo;
