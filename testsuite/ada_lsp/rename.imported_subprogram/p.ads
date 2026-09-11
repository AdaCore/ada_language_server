package P is
   function Foo return Integer
   with Import;

   X : constant Integer := Foo;
end P;
