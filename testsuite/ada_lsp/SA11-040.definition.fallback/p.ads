package P is

   procedure Foo (A : Integer; Other_Param : Boolean);

   package Nested is
      procedure Foo (A : Integer);

      procedure Bla (A : Integer);
   end Nested;

   procedure Bla (A : Integer);

end P;
