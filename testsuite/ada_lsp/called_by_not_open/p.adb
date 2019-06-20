package body P is
   function Foo return Integer is
   begin
      return Foo + 1;
   end Foo;

   task T;
   task body T is
      X : Integer;
   begin
      X := Foo + 1;
   end T;
end P;
