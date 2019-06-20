with P; use P;
procedure Main is
   function Exprfinmain return Boolean is (Foo = 43);
begin
   if Foo > 10 then
      raise Constraint_Error;
   end if;

   declare
      package Bla is
         X : Integer := Foo;
      end Bla;

      package body Bla is
         Y : Integer := Foo + 2;
      begin
         X := Foo + 3;
      end Bla;
   begin
      null;
   end;
end Main;
