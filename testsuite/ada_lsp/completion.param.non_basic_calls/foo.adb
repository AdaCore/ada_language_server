procedure Foo is
   function Bar
     (I, J, K : Integer; L : Float := 1.0) return Float is (L);
   procedure Bar (A, B : Integer) is null;
   procedure Bar (C : String);
   X : Float;
begin
   X := Bar (
end Foo;
