package body P is

   ---------
   -- Foo --
   ---------

   procedure Foo (A : Integer) is
   begin
      null;
   end Foo;

   ------------
   -- Nested --
   ------------

   package body Nested is

      procedure Foo (A : Integer) is
      begin
         null;
      end Foo;

      procedure Bla (A : Integer; Other_Param : Boolean) is null;

   end Nested;

   procedure Bla is null;

end P;
